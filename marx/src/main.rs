#![no_main]
#![warn(
	// Turn on extra language lints.
	future_incompatible,
	missing_abi,
	nonstandard_style,
	rust_2018_idioms,
	// Disabled due to <https://github.com/rust-lang/rust/issues/69952>.
	// single_use_lifetimes,
	trivial_casts,
	trivial_numeric_casts,
	unused,
	unused_crate_dependencies,
	unused_import_braces,
	// Disabled due to <https://github.com/rust-lang/rust/issues/86615>.
	// unused_lifetimes,
	unused_qualifications,

	// Turn on extra Rustdoc lints.
	rustdoc::all,

	// Turn on extra Clippy lints.
	clippy::cargo,
	clippy::pedantic,
)]
// Disabled because this isn’t going to be an actual published crate.
#![allow(clippy::cargo_common_metadata)]
// Disabled because that’s the whole point of how we use RefCell in this crate.
#![allow(clippy::await_holding_refcell_ref)]

extern crate alloc;

use alloc::borrow::ToOwned;
use alloc::boxed::Box;
use alloc::collections::BTreeMap;
use alloc::format;
use alloc::string::String;
use alloc::vec::Vec;
use core::cell::{RefCell, RefMut};
use core::cmp::min;
use core::convert::{Infallible, TryInto};
use core::num::NonZeroU32;
use core::panic::PanicInfo;
use core::time::Duration;
use futures_util::future::{select, Either};
use futures_util::pin_mut;
use minicbor::Decoder;
use oc_wasm_futures::sleep;
use oc_wasm_opencomputers::common::{Colour, Dimension, Point, RelativeSide, Rgb};
use oc_wasm_opencomputers::prelude::*;
use oc_wasm_opencomputers::robot::{ActionSide, MoveDirection, Rotation};
use oc_wasm_opencomputers::{filesystem, gpu, inventory, keyboard, redstone, robot, screen};
use oc_wasm_safe::{component, computer};

#[global_allocator]
static ALLOC: lol_alloc::AssumeSingleThreaded<lol_alloc::LeakingAllocator> =
	unsafe { lol_alloc::AssumeSingleThreaded::new(lol_alloc::LeakingAllocator::new()) };

/// A panic hook that crashes the computer, displaying the panic message.
fn panic_hook(info: &PanicInfo<'_>) {
	if let Some(s) = info.payload().downcast_ref::<&str>() {
		computer::error(&format!("panic: {s}"));
	} else if let Some(s) = info.payload().downcast_ref::<String>() {
		computer::error(&format!("panic: {s}"));
	} else {
		computer::error("panic occurred");
	}
}

/// Our own error type.
enum Error {
	/// An error returned by OpenComputers.
	OC(oc_wasm_opencomputers::error::Error),

	/// The robot’s inventory is not empty when it needs to be.
	InventoryNotEmpty,
}

impl Error {
	fn as_str(&self) -> &'static str {
		match self {
			Self::OC(e) => e.as_str(),
			Self::InventoryNotEmpty => "inventory not empty",
		}
	}
}

impl core::fmt::Display for Error {
	fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> Result<(), core::fmt::Error> {
		self.as_str().fmt(f)
	}
}

impl From<oc_wasm_safe::error::Error> for Error {
	fn from(e: oc_wasm_safe::error::Error) -> Self {
		let e: oc_wasm_opencomputers::error::Error = e.into();
		e.into()
	}
}

impl From<oc_wasm_opencomputers::error::Error> for Error {
	fn from(e: oc_wasm_opencomputers::error::Error) -> Self {
		Self::OC(e)
	}
}

/// A wrapper around `RefCell` that makes it Sync.
struct SyncRefCell<T: ?Sized>(RefCell<T>);

impl<T> SyncRefCell<T> {
	pub fn new(value: T) -> Self {
		Self(RefCell::new(value))
	}

	pub fn borrow_mut(&self) -> SyncRefMut<'_, T> {
		self.try_borrow_mut().expect("already borrowed")
	}

	pub fn try_borrow_mut(&self) -> Result<SyncRefMut<'_, T>, core::cell::BorrowMutError> {
		self.0.try_borrow_mut().map(SyncRefMut)
	}
}

// SAFETY: OC-Wasm is single-threaded.
unsafe impl<T: ?Sized> Sync for SyncRefCell<T> {}

impl<T: ?Sized> core::ops::Deref for SyncRefCell<T> {
	type Target = RefCell<T>;

	fn deref(&self) -> &RefCell<T> {
		&self.0
	}
}

/// A wrapper around `RefMut` that makes it Sync.
struct SyncRefMut<'a, T: ?Sized>(RefMut<'a, T>);

// SAFETY: OC-Wasm is single-threaded.
unsafe impl<T: ?Sized> Sync for SyncRefMut<'_, T> {}

impl<T: ?Sized> core::ops::Deref for SyncRefMut<'_, T> {
	type Target = T;

	fn deref(&self) -> &T {
		&self.0
	}
}

impl<T: ?Sized> core::ops::DerefMut for SyncRefMut<'_, T> {
	fn deref_mut(&mut self) -> &mut T {
		&mut self.0
	}
}

/// The colour displayed on the side lights when the robot is in maintenance mode.
const MAINTENANCE_COLOUR: Rgb = Rgb(0xFF_00_00);

/// The colour displayed on the side lights when the robot is working.
const BUSY_COLOUR: Rgb = Rgb(0x00_FF_00);

/// The colour displayed on the side lights when the robot needs attention in order to work.
const ATTENTION_COLOUR: Rgb = Rgb(0xFF_FF_00);

/// The percentage charge level at which we charge the robot.
const CHARGE_START_PERCENT: u32 = 50;

/// The percentage charge level at which we stop charging the robot, if we have started.
const CHARGE_STOP_PERCENT: u32 = 95;

/// The wires that control the Marx generator target voltage.
const MARX_VOLTAGE_CONTROL: WirePair = WirePair {
	high: Colour::White,
	low: Colour::Yellow,
};

/// The wires that measure the voltage on the top capacitor.
const MARX_VOLTAGE_TOP_CAP: WirePair = WirePair {
	high: Colour::Magenta,
	low: Colour::Pink,
};

/// The wires that measure the voltage on the bottom capacitor.
const MARX_VOLTAGE_BOTTOM_CAP: WirePair = WirePair {
	high: Colour::Orange,
	low: Colour::Lime,
};

/// The wire that fires the generator.
const MARX_FIRE: Colour = Colour::LightBlue;

/// The wire that measures if there are any items in the input chest.
const INPUT_CHEST: Colour = Colour::Cyan;

/// The wire that connects the high voltage power line.
const HV_CONTROL: Colour = Colour::LightGrey;

/// The side of the robot where the bundled interface is located when at the home position.
const BUNDLE_SIDE: RelativeSide = RelativeSide::Back;

/// The side of the robot where the charger is located when at the home position.
const CHARGER_SIDE: RelativeSide = RelativeSide::Left;

/// The number of ore spaces in the Marx generator.
const ORE_SLOTS: u32 = 8;

/// The number of stages in the Marx generator.
const STAGES: u32 = ORE_SLOTS + 2;

/// The capacitance per stage of the Marx generator.
const CAPACITANCE: f64 = 1.6e-6;

/// The energy per generator stage at full voltage.
#[allow(clippy::cast_possible_truncation)] // The value is known to be an integer and to fit.
#[allow(clippy::cast_sign_loss)] // The value is known to be positive.
const MAX_ENERGY_PER_STAGE: u32 = (0.5 * CAPACITANCE * 250_000.0 * 250_000.0) as u32;

/// The total generator energy at full voltage.
const MAX_TOTAL_ENERGY: u32 = MAX_ENERGY_PER_STAGE * STAGES;

/// The minimum voltage that the bottom capacitor must be charged to in order to fire at all.
const MIN_BOTTOM_VOLTAGE: f64 = 125_000.0;

/// The energy per generator stage at minimum voltage.
#[allow(clippy::cast_possible_truncation)] // The value is known to be an integer and to fit.
#[allow(clippy::cast_sign_loss)] // The value is known to be positive.
const MIN_ENERGY_PER_STAGE: u32 =
	(0.5 * CAPACITANCE * MIN_BOTTOM_VOLTAGE * MIN_BOTTOM_VOLTAGE) as u32;

/// The total generator energy at minimum voltage.
const MIN_TOTAL_ENERGY: u32 = MIN_ENERGY_PER_STAGE * STAGES;

/// Invokes a function once per timeslice until it returns success.
async fn retry<T, E>(mut f: impl FnMut() -> Result<T, E>) -> T {
	loop {
		match f() {
			Ok(x) => break x,
			Err(_) => sleep::until_next_timeslice().await,
		}
	}
}

/// The type of a pair of colours where one carries a more-significant signal and the other carries
/// a less-significant signal.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct WirePair {
	/// The more-significant wire.
	high: Colour,

	/// The less-significant wire.
	low: Colour,
}

/// The singleton resources that must be passed around.
struct Resources {
	/// The component lister.
	lister: component::Lister,

	/// The component method invoker.
	invoker: component::Invoker,

	/// A byte buffer used for invoking methods.
	buffer: Vec<u8>,
}

impl Resources {
	/// Obtains the one and only copy of the resources.
	fn take() -> Self {
		let buffer = Vec::with_capacity(4096);
		Self {
			lister: component::Lister::take().unwrap(),
			invoker: component::Invoker::take().unwrap(),
			buffer,
		}
	}
}

/// Overall static information about the system which can be gathered at startup and does not
/// change at runtime.
struct SysInfo {
	/// The GPU.
	gpu: gpu::Gpu,

	/// The screen.
	screen: screen::Screen,

	/// The dimensions of the screen.
	screen_size: Dimension,

	/// The redstone component.
	redstone: redstone::Redstone,

	/// The inventory controller component.
	inventory: inventory::Controller,

	/// The robot component.
	robot: robot::Robot,

	/// The filesystem component for the hard disk.
	filesystem: filesystem::Filesystem,
}

impl SysInfo {
	/// Collects the static information.
	async fn new(resources: &mut Resources) -> Result<Self, Error> {
		let gpu = gpu::Gpu::new(
			*resources
				.lister
				.start(Some(gpu::TYPE))
				.next()
				.expect("missing GPU")
				.address(),
		);
		let screen = screen::Screen::new(
			*resources
				.lister
				.start(Some(screen::TYPE))
				.next()
				.expect("missing screen")
				.address(),
		);
		let screen_size = gpu
			.lock(&mut resources.invoker, &mut resources.buffer)
			.max_resolution()
			.await?;
		let redstone = redstone::Redstone::new(
			*resources
				.lister
				.start(Some(redstone::TYPE))
				.next()
				.expect("missing redstone card")
				.address(),
		);
		let inventory = inventory::Controller::new(
			*resources
				.lister
				.start(Some(inventory::INVENTORY_CONTROLLER_TYPE))
				.next()
				.expect("missing inventory controller upgrade")
				.address(),
		);
		let robot = robot::Robot::new(
			*resources
				.lister
				.start(Some(robot::TYPE))
				.next()
				.expect("missing robot component")
				.address(),
		);
		let filesystem = filesystem::Filesystem::new(
			*resources
				.lister
				.start(Some(filesystem::TYPE))
				.next()
				.expect("missing filesystem component")
				.address(),
		);
		Ok(Self {
			gpu,
			screen,
			screen_size,
			redstone,
			inventory,
			robot,
			filesystem,
		})
	}
}

/// Each element of the contained map maps from the Minecraft-internal name of an ore to the
/// energy, in Joules, required for one block of that ore.
struct Database(BTreeMap<String, u32>);

impl Database {
	/// The filename of the database.
	const FILENAME: &'static str = "marx.db";

	/// Creates a new, empty database.
	fn new() -> Self {
		Self(BTreeMap::new())
	}

	/// Loads the database from disk.
	async fn load(resources: &mut Resources, sys_info: &SysInfo) -> Result<Option<Self>, Error> {
		let mut fs_locked = sys_info
			.filesystem
			.lock(&mut resources.invoker, &mut resources.buffer);
		let handle = fs_locked.open_read(Self::FILENAME).await;
		if let Ok(handle) = handle {
			let mut map = BTreeMap::new();
			while let Some(name_length) = (&handle)
				.lock(&mut resources.invoker, &mut resources.buffer)
				.read(1)
				.await?
			{
				let name_length = name_length[0] as usize;
				let name = (&handle)
					.lock(&mut resources.invoker, &mut resources.buffer)
					.read(name_length)
					.await?
					.expect("corrupt database");
				let name = core::str::from_utf8(name)
					.expect("corrupt database")
					.to_owned();
				let value = (&handle)
					.lock(&mut resources.invoker, &mut resources.buffer)
					.read((u32::BITS / 8) as usize)
					.await?
					.expect("corrupt database");
				let value: &[u8; (u32::BITS / 8) as usize] =
					value.try_into().expect("corrupt database");
				let value = u32::from_be_bytes(*value);
				map.insert(name, value);
			}
			Ok(Some(Self(map)))
		} else {
			Ok(None)
		}
	}

	/// Saves the database to disk.
	async fn save(&self, resources: &mut Resources, sys_info: &SysInfo) -> Result<(), Error> {
		let mut fs_locked = sys_info
			.filesystem
			.lock(&mut resources.invoker, &mut resources.buffer);
		let handle = fs_locked
			.open_write(Self::FILENAME, filesystem::WriteMode::Truncate)
			.await?;
		let mut handle = (&handle).lock(&mut resources.invoker, &mut resources.buffer);
		for (name, value) in &self.0 {
			let name = name.as_bytes();
			let name_len: u8 = name.len().try_into().expect("item name too long");
			handle.write(&[name_len]).await?;
			handle.write(name).await?;
			handle.write(&value.to_be_bytes()).await?;
		}
		Ok(())
	}
}

/// An item count and an energy level.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct CountAndEnergy {
	/// The number of items.
	count: u32,

	/// The energy, in joules.
	energy: u32,
}

/// A high/low pair of redstone signal levels.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct RedstonePair {
	/// The high value, in Project Red units.
	high: u8,

	/// The low value, in Project Red units.
	low: u8,
}

impl RedstonePair {
	/// Constructs a new `RedstonePair` from a voltage.
	fn from_voltage(voltage: f64) -> Self {
		// We clamp it to [0, 255].
		#[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
		let combined_redstone = ((voltage / 250_000.0) * 255.0).round().clamp(0.0, 255.0) as u8;
		let vanilla_high = combined_redstone >> 4;
		let vanilla_low = combined_redstone & 15;
		let project_red_high = vanilla_high * 17;
		let project_red_low = vanilla_low * 17;
		Self {
			high: project_red_high,
			low: project_red_low,
		}
	}

	/// Constructs a new `RedstonePair` by extracting two levels (identified by a
	/// [`WirePair`](WirePair)) from a redstone sample.
	fn from_wires(sample: &[u8; 16], wires: WirePair) -> Self {
		let high = sample[usize::from(wires.high)];
		let low = sample[usize::from(wires.low)];
		Self { high, low }
	}

	/// Converts a `RedstonePair` to a voltage.
	fn as_voltage(self) -> f64 {
		let vanilla_high = self.high / 17;
		let vanilla_low = self.low / 17;
		(250_000.0 / 255.0) * f64::from(16 * vanilla_high + vanilla_low)
	}

	/// Converts a `RedstonePair` to an 8-bit value representing its effective vanilla signal
	/// strengths, packed.
	fn as_vanilla_packed(self) -> u8 {
		let vanilla_high = self.high / 17;
		let vanilla_low = self.low / 17;
		(vanilla_high << 4) | vanilla_low
	}
}

/// The full state needed to run the application.
struct Application {
	/// The resources, which can be held by one consumer at a time.
	resources: SyncRefCell<Resources>,

	/// The system information, which is immutable and can be used by many consumers at once.
	sys_info: SysInfo,

	/// The database of energy amounts needed by ore type.
	database: SyncRefCell<Database>,
}

impl Application {
	/// Initializes the application.
	async fn new() -> Result<Application, Error> {
		// Grab resources.
		let mut resources = Resources::take();

		// Collect system info.
		let sys_info = SysInfo::new(&mut resources).await?;

		// Load the database. If it doesn’t exist, create an empty one.
		let database = Database::load(&mut resources, &sys_info)
			.await?
			.unwrap_or_else(Database::new);

		Ok(Self {
			resources: SyncRefCell::new(resources),
			sys_info,
			database: SyncRefCell::new(database),
		})
	}

	/// Waits until the resources are available and obtains them.
	async fn resources(&self) -> SyncRefMut<'_, Resources> {
		retry(|| self.resources.try_borrow_mut()).await
	}

	/// Clears the screen and displays a multi-line message, centred.
	async fn show_message(&self, message: &[&str]) -> Result<(), Error> {
		let resources = &mut *self.resources().await;
		let mut gpu_locked = self
			.sys_info
			.gpu
			.lock(&mut resources.invoker, &mut resources.buffer);
		gpu_locked
			.fill(Point { x: 1, y: 1 }, self.sys_info.screen_size, ' ')
			.await?;
		// message.len() is small, it must fit on the screen
		#[allow(clippy::cast_possible_truncation)]
		let first_row = (self.sys_info.screen_size.height - message.len() as u32) / 2 + 1;
		for (i, line) in message.iter().enumerate() {
			let first_col = (self.sys_info.screen_size.width
				- line.chars().map(computer::char_width).sum::<u32>())
				/ 2_u32 + 1_u32;
			#[allow(clippy::cast_possible_truncation)] // i is small because message.len() is small
			gpu_locked
				.set(
					Point {
						x: first_col,
						y: first_row + i as u32,
					},
					line,
					gpu::TextDirection::Horizontal,
				)
				.await?;
		}
		Ok(())
	}

	/// Returns the next key the user types.
	async fn get_key(&self) -> Result<keyboard::BasicKeySignal, Error> {
		Ok(loop {
			sleep::until_signal().await;
			if let Some(len) = computer::pull_signal_length() {
				let resources = &mut *self.resources().await;
				resources.buffer.resize(len.get(), 0);
				let signal = computer::pull_signal(&mut resources.buffer)?.unwrap();
				let mut decoder = Decoder::new(signal);
				let signal_name = decoder
					.str()
					.map_err(|_| oc_wasm_safe::error::Error::CborDecode)?;
				if signal_name == keyboard::KeySignal::KEY_DOWN {
					let params: keyboard::KeySignal<'_> = decoder
						.decode()
						.map_err(|_| oc_wasm_safe::error::Error::CborDecode)?;
					let params = params.to_basic();
					break params;
				}
			}
		})
	}

	/// Checks the battery charge level and recharges the battery if needed.
	///
	/// This future completes immediately if no charge cycle is needed; otherwise, it completes
	/// once the charge cycle finishes.
	///
	/// The robot must be at home position.
	async fn charge_cycle(&self) -> Result<(), Error> {
		// Check if charging is needed.
		if Self::check_charge_condition() == Some(true) {
			// Start charging.
			self.enable_charger(true).await?;

			// Check the charge level every second until full.
			while Self::check_charge_condition() != Some(false) {
				sleep::for_uptime(Duration::from_secs(1)).await;
			}

			// Stop charging.
			self.enable_charger(false).await?;
		}
		Ok(())
	}

	/// Checks whether the robot should start or stop charging.
	///
	/// If the robot should start charging, `Some(true)` is returned. If the robot should stop
	/// charging, `Some(false)` is returned. If the robot should remain in its current state
	/// (remain charging if it already was, but do not start), `None` is returned.
	fn check_charge_condition() -> Option<bool> {
		#[allow(clippy::cast_possible_truncation)] // energy levels are small enough to fit in a u32
		#[allow(clippy::cast_sign_loss)] // energy levels are nonnegative
		let charge_percent = computer::energy().into_inner() as u32 * 100
			/ computer::max_energy().into_inner() as u32;
		if charge_percent < CHARGE_START_PERCENT {
			Some(true)
		} else if charge_percent > CHARGE_STOP_PERCENT {
			Some(false)
		} else {
			None
		}
	}

	/// Enables or disables the battery charger.
	///
	/// The robot must be at home position.
	async fn enable_charger(&self, charge: bool) -> Result<(), Error> {
		let resources = &mut *self.resources().await;
		self.sys_info
			.redstone
			.lock(&mut resources.invoker, &mut resources.buffer)
			.set_side_output(CHARGER_SIDE, if charge { 15 } else { 0 })
			.await?;
		Ok(())
	}

	/// Monitors the battery charge level forever and turns the battery charger on or off as
	/// needed.
	///
	/// The robot must be at home position.
	async fn charge_forever(&self) -> Result<Infallible, Error> {
		loop {
			// Charge, if needed.
			self.charge_cycle().await?;

			// At this point we know the battery is above the low threshold—either a charge cycle
			// just finished, or the cycle was skipped because the battery was not low enough. In
			// either case, sleep for a minute before checking again.
			sleep::for_uptime(Duration::from_secs(60)).await;
		}
	}

	/// Displays, for one second, a message asking the operator whether to enter maintenance
	/// lockout, and checks whether they wish to do so.
	async fn check_maintenance_lockout(&self) -> Result<bool, Error> {
		const MAINTENANCE_MESSAGE: &[&str] =
			&["Press L key to lock", "system out for", "maintenance"];

		// Show the message.
		self.show_message(MAINTENANCE_MESSAGE).await?;

		// Wait for up to a second or until the user presses L.
		let timeout = sleep::for_uptime(Duration::from_secs(1));
		pin_mut!(timeout);
		let key = async {
			loop {
				let key = self.get_key().await?;
				if key.character == Some('l') || key.character == Some('L') {
					break Ok::<_, Error>(());
				}
			}
		};
		pin_mut!(key);
		match select(timeout, key).await {
			Either::Left(((), _)) => Ok(false),
			Either::Right((Ok(()), _)) => Ok(true),
			Either::Right((Err(e), _)) => Err(e),
		}
	}

	/// Handles the maintenance lockout condition.
	async fn maintenance_lockout(&self) -> Result<(), Error> {
		const MAINTENANCE_MESSAGE: &[&str] = &[
			"The system is locked",
			"out for maintenance.",
			"",
			"Press U to unlock.",
		];

		let mut resources_locked = self.resources().await;
		let resources = &mut *resources_locked;

		// Set the side light colour.
		self.sys_info
			.robot
			.lock(&mut resources.invoker, &mut resources.buffer)
			.set_light_colour(MAINTENANCE_COLOUR)
			.await?;

		// Disable the redstone wakeup threshold so that, even if the robot runs out of energy while
		// maintenance is under way, and then gets charged up again, it won’t turn back on due to
		// redstone signalling and go into normal mode on its own.
		self.sys_info
			.redstone
			.lock(&mut resources.invoker, &mut resources.buffer)
			.set_wake_threshold(0)
			.await?;

		drop(resources_locked);

		// Show the message.
		self.show_message(MAINTENANCE_MESSAGE).await?;

		// Wait until the user presses U.
		loop {
			let key = self.get_key().await?;
			if key.character == Some('u') || key.character == Some('U') {
				break;
			}
		}

		Ok(())
	}

	/// Reads a line of input from the user.
	async fn read_line(&self, line: u32) -> Result<String, Error> {
		// Clear the prompt line and print the prompt and cursor.
		let mut resources_locked = self.resources().await;
		let resources = &mut *resources_locked;
		let mut gpu_locked = self
			.sys_info
			.gpu
			.lock(&mut resources.invoker, &mut resources.buffer);
		gpu_locked
			.fill(
				Point { x: 1, y: line },
				Dimension {
					width: self.sys_info.screen_size.width,
					height: 1,
				},
				' ',
			)
			.await?;
		gpu_locked
			.set(
				Point { x: 1, y: line },
				"> \u{2588}",
				gpu::TextDirection::Horizontal,
			)
			.await?;
		drop(resources_locked);

		// Record the string typed so far and the column number of the cursor.
		let mut data = String::new();
		let mut cursor_col = 3;

		// Handle keys.
		loop {
			let params = self.get_key().await?;
			if let Some(ch) = params.character {
				let mut resources_locked = self.resources().await;
				let resources = &mut *resources_locked;
				if ch == '\u{8}' {
					// This is a backspace. If there are any characters in the typed string, remove
					// the last one, move the cursor accordingly, and reprint the cursor,
					// overwriting the deleted character.
					if let Some(last) = data.pop() {
						cursor_col -= computer::char_width(last);
						self.sys_info
							.gpu
							.lock(&mut resources.invoker, &mut resources.buffer)
							.set(
								Point {
									x: cursor_col,
									y: line,
								},
								"\u{2588}     ",
								gpu::TextDirection::Horizontal,
							)
							.await?;
					}
				} else if ch == '\r' || ch == '\n' {
					// This is an Enter. Break out of the loop and return the typed line.
					break;
				} else {
					// This is another character. Store and print it and update the cursor
					// position.
					data.push(ch);
					self.sys_info
						.gpu
						.lock(&mut resources.invoker, &mut resources.buffer)
						.set(
							Point {
								x: cursor_col,
								y: line,
							},
							&format!("{ch}\u{2588}"),
							gpu::TextDirection::Horizontal,
						)
						.await?;
					cursor_col += computer::char_width(ch);
				}
			}
		}

		// Return the typed line.
		Ok(data)
	}

	/// Asks the user how much energy an ore requires.
	///
	/// This function expects to be called with, and returns with, the robot at home position and
	/// displaying the busy colour on its side lights.
	async fn ask_ore_energy(&self, label: &str) -> Result<u32, Error> {
		{
			let resources = &mut *self.resources().await;

			// Set colour to attention colour.
			self.sys_info
				.robot
				.lock(&mut resources.invoker, &mut resources.buffer)
				.set_light_colour(ATTENTION_COLOUR)
				.await?;

			// Clear the screen and show a message.
			let mut gpu_locked = self
				.sys_info
				.gpu
				.lock(&mut resources.invoker, &mut resources.buffer);
			gpu_locked
				.fill(Point { x: 1, y: 1 }, self.sys_info.screen_size, ' ')
				.await?;
			for (i, line) in [
				"Unknown ore found:",
				label,
				"",
				"Enter joules required per ore block:",
			]
			.iter()
			.enumerate()
			{
				#[allow(clippy::cast_possible_truncation)] // Text line numbers are always small.
				gpu_locked
					.set(
						Point {
							x: 1,
							y: (i + 1) as u32,
						},
						line,
						gpu::TextDirection::Horizontal,
					)
					.await?;
			}
		}

		// Try to get the user’s attention.
		computer::beep_pattern("-");

		// Let the user type.
		let energy = loop {
			let line = self.read_line(6).await?;
			if let Ok(e) = line.parse() {
				break e;
			}
		};

		// Clear the screen.
		let resources = &mut *self.resources().await;
		self.sys_info
			.gpu
			.lock(&mut resources.invoker, &mut resources.buffer)
			.fill(Point { x: 1, y: 1 }, self.sys_info.screen_size, ' ')
			.await?;

		// Set colour to work colour.
		self.sys_info
			.robot
			.lock(&mut resources.invoker, &mut resources.buffer)
			.set_light_colour(BUSY_COLOUR)
			.await?;

		Ok(energy)
	}

	/// Scans the chest. Returns a vector of [`CountAndEnergy`](CountAndEnergy) values, one for
	/// each slot in the chest.
	///
	/// If an unknown item is encountered, the user is prompted for its energy level.
	///
	/// This function expects to be called with, and returns with, the robot at home position.
	async fn scan_chest(&self) -> Result<Vec<CountAndEnergy>, Error> {
		let mut database_locked = self.database.borrow_mut();
		let database = &mut *database_locked;
		let mut resources_locked = self.resources().await;
		let resources = &mut *resources_locked;
		let mut chest_contents = Vec::new();
		let snapshot = self
			.sys_info
			.inventory
			.lock(&mut resources.invoker, &mut resources.buffer)
			.get_all_stacks(robot::ActionSide::Front)
			.await?;
		let count = (&snapshot)
			.lock(&mut resources.invoker, &mut resources.buffer)
			.count()
			.await?;
		chest_contents.reserve(count as usize);
		drop(resources_locked);
		for _ in 0..count {
			let mut resources_locked = self.resources().await;
			let resources = &mut *resources_locked;
			let stack = (&snapshot)
				.lock(&mut resources.invoker, &mut resources.buffer)
				.next()
				.await?;
			if let Some(stack) = stack {
				// Look up the item in the database.
				let count = stack.size;
				let energy = if let Some(&e) = database.0.get(stack.name) {
					e
				} else {
					// It’s not there. Ask the user about it. Keep the battery charged while doing so.
					let name = stack.name.to_owned();
					let label = stack.label.to_owned();
					drop(resources_locked);
					let e = {
						let f1 = self.ask_ore_energy(&label);
						pin_mut!(f1);
						let f2 = self.charge_forever();
						pin_mut!(f2);
						let ret = select(f1, f2).await;
						self.enable_charger(false).await?;
						match ret {
							Either::Left((Ok(n), _)) => n,
							Either::Left((Err(e), _)) | Either::Right((Err(e), _)) => {
								return Err(e)
							}
							Either::Right((Ok(_), _)) => panic!("infallible"),
						}
					};

					// Put the new value in the database and save it before proceeding.
					database.0.insert(name, e);
					let mut resources_locked = self.resources().await;
					let resources = &mut *resources_locked;
					database.save(resources, &self.sys_info).await?;
					e
				};
				chest_contents.push(CountAndEnergy { count, energy });
			} else {
				// This slot in the chest is empty.
				chest_contents.push(CountAndEnergy {
					count: 0,
					energy: 0,
				});
			}
		}
		Ok(chest_contents)
	}

	/// Given the available items, chooses the best energy level for a cycle.
	///
	/// Because energy is divided evenly among the ore blocks, all ore processed in a single firing
	/// must have the same required energy level. However they do not have to be the same item—two
	/// different ores that both require the same amount of energy can be fired at the same time.
	///
	/// The best energy level is the one that matches the largest number of items. If no items at
	/// all are present, or if the number and type of items is such that the minimum firing energy
	/// cannot be achieved, `None` is returned.
	fn choose_energy_level(chest_contents: &[CountAndEnergy]) -> Option<u32> {
		// Collect a record for each distinct energy level, along with the count of items in the
		// entire chest that require that energy.
		let mut distinct_energy_levels: Vec<CountAndEnergy> = Vec::new();
		for chest_slot in chest_contents {
			if let Some(record) = distinct_energy_levels
				.iter_mut()
				.find(|i| i.energy == chest_slot.energy)
			{
				// distinct_energy_levels already contains an entry for this energy level, so add this
				// chest slot’s count to that entry.
				record.count += chest_slot.count;
			} else {
				// distinct_energy_levels does not contain an entry for this energy level, so add one.
				distinct_energy_levels.push(*chest_slot);
			}
		}

		// Return the energy level whose count is maximum, filtering out elements that do not meet
		// the minimum total energy requirement.
		distinct_energy_levels
			.into_iter()
			.filter(|i| i.energy * i.count >= MIN_TOTAL_ENERGY)
			.max_by(|x, y| x.count.cmp(&y.count))
			.map(|i| i.energy)
	}

	/// Collects items into the robot’s internal inventory.
	///
	/// The items collected are all items that require the specified energy level. The number of
	/// items collected is the lesser of [`ORE_SLOTS`](ORE_SLOTS), the number of ores of the
	/// specified energy level that the Marx generator can hold enough energy to process, and the
	/// number of ores of the specified energy level available in the input chest.
	///
	/// This function expects to be called with, and returns with, the robot at home position.
	async fn collect_items(
		&self,
		chest_contents: &[CountAndEnergy],
		energy_per_ore: u32,
	) -> Result<(), Error> {
		let mut resources_locked = self.resources().await;
		let resources = &mut *resources_locked;
		let max_ores_due_to_energy = MAX_TOTAL_ENERGY / energy_per_ore;
		let mut remaining = min(ORE_SLOTS, max_ores_due_to_energy);
		for (slot, content) in chest_contents.iter().enumerate() {
			if content.energy == energy_per_ore && remaining != 0 {
				#[allow(clippy::cast_possible_truncation)] // Inventory slot numbers are small.
				let sucked = self
					.sys_info
					.inventory
					.lock(&mut resources.invoker, &mut resources.buffer)
					.suck_from_slot(
						robot::ActionSide::Front,
						NonZeroU32::new(slot as u32 + 1).unwrap(),
						remaining,
						None,
					)
					.await?;
				remaining -= sucked;
			}
		}
		Ok(())
	}

	/// Places ore from the robot’s internal inventory between the Marx generator’s electrodes.
	///
	/// The number of ore blocks placed is returned.
	///
	/// This function expects to be called with, and returns with, the robot at home position.
	async fn place_ore(&self) -> Result<u32, Error> {
		let mut resources_locked = self.resources().await;
		let resources = &mut *resources_locked;

		// Move to the electrodes.
		let mut robot = self
			.sys_info
			.robot
			.lock(&mut resources.invoker, &mut resources.buffer);
		robot.turn(Rotation::Clockwise).await?;
		robot.move_robot(MoveDirection::Front).await?;
		robot.move_robot(MoveDirection::Front).await?;
		robot.turn(Rotation::Clockwise).await?;
		robot.move_robot(MoveDirection::Front).await?;
		robot.turn(Rotation::Counterclockwise).await?;
		robot.move_robot(MoveDirection::Up).await?;
		robot.move_robot(MoveDirection::Front).await?;
		robot.move_robot(MoveDirection::Front).await?;
		robot.move_robot(MoveDirection::Front).await?;
		robot.move_robot(MoveDirection::Front).await?;
		robot.move_robot(MoveDirection::Front).await?;
		robot.move_robot(MoveDirection::Front).await?;
		robot.move_robot(MoveDirection::Front).await?;
		robot.move_robot(MoveDirection::Front).await?;
		robot.turn(Rotation::Counterclockwise).await?;

		// Place the ore.
		let mut ore_placed = 0;
		for i in 1..=robot.inventory_size().await? {
			robot.select(NonZeroU32::new(i).unwrap()).await?;
			for _ in 0..robot.count_selected().await? {
				if ore_placed != 0 {
					robot.move_robot(MoveDirection::Up).await?;
				}
				robot.place(ActionSide::Front, None, false).await?;
				ore_placed += 1;
			}
		}
		let ore_placed = ore_placed;

		// Come back down.
		for _ in 0..(ore_placed - 1) {
			robot.move_robot(MoveDirection::Down).await?;
		}

		// Go home.
		robot.turn(Rotation::Counterclockwise).await?;
		robot.move_robot(MoveDirection::Front).await?;
		robot.move_robot(MoveDirection::Front).await?;
		robot.move_robot(MoveDirection::Front).await?;
		robot.move_robot(MoveDirection::Front).await?;
		robot.move_robot(MoveDirection::Front).await?;
		robot.move_robot(MoveDirection::Front).await?;
		robot.move_robot(MoveDirection::Front).await?;
		robot.move_robot(MoveDirection::Front).await?;
		robot.move_robot(MoveDirection::Down).await?;
		robot.turn(Rotation::Clockwise).await?;
		robot.move_robot(MoveDirection::Front).await?;
		robot.turn(Rotation::Counterclockwise).await?;
		robot.move_robot(MoveDirection::Front).await?;
		robot.move_robot(MoveDirection::Front).await?;
		robot.turn(Rotation::Clockwise).await?;

		Ok(ore_placed)
	}

	/// Enables or disables the high voltage switch.
	///
	/// This function expects to be called with, and returns with, the robot at home position.
	async fn enable_hv(&self, enable: bool) -> Result<(), Error> {
		let resources = &mut *self.resources().await;
		let mut redstone = self
			.sys_info
			.redstone
			.lock(&mut resources.invoker, &mut resources.buffer);
		redstone
			.set_side_colour_bundled_output(BUNDLE_SIDE, HV_CONTROL, if enable { 255 } else { 0 })
			.await?;
		Ok(())
	}

	/// Charges up the Marx generator.
	///
	/// This function expects to be called with, and returns with, the robot at home position and
	/// displaying the busy colour on its side lights. It also expects the high voltage switch to
	/// be closed.
	async fn charge_marx(&self, energy: u32) -> Result<(), Error> {
		// Calculate required voltage and convert to two redstone signals.
		let target = {
			let energy_per_stage = energy / STAGES;
			let voltage = (2.0 * f64::from(energy_per_stage) / CAPACITANCE).sqrt();
			RedstonePair::from_voltage(voltage)
		};

		// Set the voltage setpoint.
		{
			let mut levels = [None; 16];
			levels[usize::from(MARX_VOLTAGE_CONTROL.high)] = Some(target.high);
			levels[usize::from(MARX_VOLTAGE_CONTROL.low)] = Some(target.low);
			let resources = &mut *self.resources().await;
			self.sys_info
				.redstone
				.lock(&mut resources.invoker, &mut resources.buffer)
				.set_side_bundled_output(BUNDLE_SIDE, &levels)
				.await?;
		}

		// Wait until we get there. While doing so, recharge the robot’s battery when it needs it.
		{
			let charge_robot_fut = self.charge_forever();
			pin_mut!(charge_robot_fut);
			let charge_marx_fut = async {
				loop {
					// Read the redstone levels.
					let levels = {
						let resources = &mut *self.resources().await;
						self.sys_info
							.redstone
							.lock(&mut resources.invoker, &mut resources.buffer)
							.get_side_bundled_input(BUNDLE_SIDE)
							.await?
					};
					let top = RedstonePair::from_wires(&levels, MARX_VOLTAGE_TOP_CAP);
					let bottom = RedstonePair::from_wires(&levels, MARX_VOLTAGE_BOTTOM_CAP);

					// If we’re finished charging, break out of the loop now. For the top
					// capacitor, since it charges quite slowly, consider two LSbs below full to be
					// good enough.
					let bottom_done = bottom == target;
					let top_done =
						top.as_vanilla_packed().saturating_add(2) >= target.as_vanilla_packed();
					if bottom_done && top_done {
						break Ok::<_, Error>(());
					}

					// No, we’re not finished charging yet. Show the situation on-screen.
					let line1 = format!("Target: {:6.0} V", target.as_voltage());
					let line2 = format!("Bottom: {:6.0} V", bottom.as_voltage());
					let line3 = format!("Top:    {:6.0} V", top.as_voltage());
					self.show_message(&[&line1, &line2, &line3]).await?;

					// Wait until a signal arrives (as it will when a redstone input changes).
					sleep::until_signal().await;

					// Pop and discard any signals.
					let resources = &mut *self.resources().await;
					while let Some(len) = computer::pull_signal_length() {
						resources.buffer.resize(len.get(), 0);
						computer::pull_signal(&mut resources.buffer).unwrap();
					}
				}
			};
			pin_mut!(charge_marx_fut);
			match select(charge_robot_fut, charge_marx_fut).await {
				Either::Left((Ok(_), _)) => panic!("infallible"),
				Either::Left((Err(e), _)) | Either::Right((Err(e), _)) => return Err(e),
				Either::Right((Ok(()), _)) => (),
			}
		}

		// Turn the robot battery charge signal off, in case it happened to be on when the
		// capacitors finished charging.
		{
			let resources = &mut *self.resources().await;
			self.sys_info
				.redstone
				.lock(&mut resources.invoker, &mut resources.buffer)
				.set_side_output(CHARGER_SIDE, 0)
				.await?;
		}

		Ok(())
	}

	/// Performs the bulk of the work for a normal cycle.
	///
	/// This function returns `true` if it did some work, or `false` if it discovered there were
	/// not enough items to do anything.
	///
	/// This function expects to be called with, and returns with, the robot at home position and
	/// displaying the busy colour on its side lights.
	async fn normal_cycle_impl(&self) -> Result<bool, Error> {
		// Scan the chest.
		let chest_contents = self.scan_chest().await?;

		// Choose the energy level to use on this cycle.
		let Some(energy_per_ore) = Self::choose_energy_level(&chest_contents) else {
			// No items, or not enough to reach minimum energy → don’t run a cycle.
			return Ok(false);
		};

		// Collect up to ORE_SLOTS items of the selected energy level into the robot’s internal
		// inventory, but also limited by how much energy we can deliver.
		self.collect_items(&chest_contents, energy_per_ore).await?;

		// Place the ore.
		let ore_placed = self.place_ore().await?;

		// Close the high voltage switch.
		self.enable_hv(true).await?;

		// Charge the generator.
		let charge_result = self.charge_marx(energy_per_ore * ore_placed).await;

		// Check success of the charge operation now that the high voltage switch is open for
		// safety in the event of failure.
		charge_result?;

		// Open the high voltage switch.
		self.enable_hv(false).await?;

		// Fire the generator.
		let resources = &mut *self.resources().await;
		let mut redstone = self
			.sys_info
			.redstone
			.lock(&mut resources.invoker, &mut resources.buffer);
		redstone
			.set_side_colour_bundled_output(BUNDLE_SIDE, MARX_FIRE, 255)
			.await?;
		sleep::for_uptime(Duration::from_secs(1)).await;
		redstone
			.set_side_colour_bundled_output(BUNDLE_SIDE, MARX_FIRE, 0)
			.await?;

		Ok(true)
	}

	/// Performs a normal cycle.
	///
	/// This function returns `true` if it did some work, or `false` if it discovered there were
	/// not enough items to do anything.
	///
	/// This function expects to be called with, and returns with, the robot at home position and
	/// the side lights off.
	async fn normal_cycle(&self) -> Result<bool, Error> {
		// Check whether we need to charge the robot before starting work.
		self.charge_cycle().await?;

		// Set colour to working colour.
		{
			let mut resources_locked = self.resources().await;
			let resources = &mut *resources_locked;
			self.sys_info
				.robot
				.lock(&mut resources.invoker, &mut resources.buffer)
				.set_light_colour(BUSY_COLOUR)
				.await?;
		}

		// Run the cycle.
		let ret = self.normal_cycle_impl().await;

		// Turn the side lights off until we decide what to do next.
		{
			let mut resources_locked = self.resources().await;
			let resources = &mut *resources_locked;
			self.sys_info
				.robot
				.lock(&mut resources.invoker, &mut resources.buffer)
				.set_light_colour(Rgb(0))
				.await?;
		}

		ret
	}

	/// Runs the application.
	async fn main(&self) -> Result<Infallible, Error> {
		let mut resources_locked = self.resources().await;
		let resources = &mut *resources_locked;

		// Turn off the side light until we decide what we’re doing.
		self.sys_info
			.robot
			.lock(&mut resources.invoker, &mut resources.buffer)
			.set_light_colour(Rgb(0))
			.await?;

		// Bind the GPU to the screen.
		let mut gpu_locked = self
			.sys_info
			.gpu
			.lock(&mut resources.invoker, &mut resources.buffer);
		gpu_locked
			.bind(*self.sys_info.screen.address(), true)
			.await?;
		let screen_size = gpu_locked.max_resolution().await?;
		gpu_locked.set_resolution(screen_size).await?;

		// Clear all redstone outputs which might be left over from a previous run.
		let mut rs_locked = self
			.sys_info
			.redstone
			.lock(&mut resources.invoker, &mut resources.buffer);
		rs_locked.set_output(&[Some(0); 6]).await?;
		rs_locked.set_bundled_output(&[[Some(0); 16]; 6]).await?;

		// Check that our first inventory slot is empty and the others are not.
		let mut robot_locked = self
			.sys_info
			.robot
			.lock(&mut resources.invoker, &mut resources.buffer);
		for i in 1..=robot_locked.inventory_size().await? {
			if robot_locked.count(NonZeroU32::new(i).unwrap()).await? != 0 {
				return Err(Error::InventoryNotEmpty);
			}
		}

		// Startup is finished. Release the resources; they will be borrowed with finer granularity in
		// future as needed.
		drop(resources_locked);

		loop {
			// Configure a wakeup threshold of 1, so that when the chest level sensing wire detects items
			// in the chest, the robot will power up if it is shut down.
			{
				let resources = &mut *self.resources().await;
				self.sys_info
					.redstone
					.lock(&mut resources.invoker, &mut resources.buffer)
					.set_wake_threshold(1)
					.await?;
			}

			// Decide what to do.
			if self.check_maintenance_lockout().await? {
				// The user asked to go into maintenance lockout mode. Do that, while also monitoring
				// the battery and recharging if needed simultaneously.
				let f1 = self.maintenance_lockout();
				pin_mut!(f1);
				let f2 = async {
					self.charge_forever().await?;
					Ok(())
				};
				pin_mut!(f2);
				select(f1, f2).await.factor_first().0?;
			} else {
				let mut resources_locked = self.resources().await;
				let resources = &mut *resources_locked;

				// The user did not ask to go into maintenance mode. Clear the screen.
				self.sys_info
					.gpu
					.lock(&mut resources.invoker, &mut resources.buffer)
					.fill(Point { x: 1, y: 1 }, self.sys_info.screen_size, ' ')
					.await?;

				// Check whether the comparator signal indicates there are any items in the chest.
				let items_in_chest = self
					.sys_info
					.redstone
					.lock(&mut resources.invoker, &mut resources.buffer)
					.get_side_colour_bundled_input(BUNDLE_SIDE, INPUT_CHEST)
					.await? != 0;

				drop(resources_locked);

				if items_in_chest {
					// There are items to process.
					let processed_any = self.normal_cycle().await?;
					if !processed_any {
						// There are some items in the chest, but not enough to meet the minimum
						// energy requirement and accomplish anything. We can’t power down because
						// wake-on-redstone will not wake us up again because the chest is
						// nonempty. Instead, go to sleep for a few seconds and then check the
						// chest again; this should be a short-lived situation because the Applied
						// Energistics network should deliver items in usable quantities.
						sleep::for_uptime(Duration::from_secs(3)).await;
					}
				} else {
					// The redstone signal from the input chest indicates that the chest is empty. Turn
					// off the robot to save energy and rely on the wake threshold (or the power
					// button) to turn it back on when needed.
					computer::shutdown();
				}
			}
		}
	}
}

/// Runs the application.
async fn main_impl() -> Result<Infallible, Error> {
	let app = Application::new().await?;
	app.main().await
}

/// Runs the application, displaying any error if one is returned.
async fn main() -> Infallible {
	// Set the panic hook.
	std::panic::set_hook(Box::new(panic_hook));

	// Run the main function and report errors if it returns one.
	match main_impl().await {
		Ok(i) => i,
		Err(e) => computer::error(e.as_str()),
	}
}

#[no_mangle]
pub extern "C" fn run(arg: i32) -> i32 {
	oc_wasm_cassette::run(arg, main)
}
