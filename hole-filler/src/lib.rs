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
	unused_lifetimes,
	unused_qualifications,

	// Turn on extra Rustdoc lints.
	rustdoc::all,

	// Turn on extra Clippy lints.
	clippy::cargo,
	clippy::pedantic,
)]
// Disabled because this isn’t going to be an actual published crate.
#![allow(clippy::cargo_common_metadata)]

use core::cmp::Ordering;
use core::convert::Infallible;
use core::num::NonZeroU32;
use core::panic::PanicInfo;
use core::time::Duration;
use minicbor::decode::Decoder;
use oc_wasm_futures::sleep;
use oc_wasm_opencomputers::common::{Dimension, Lockable, Point, Rgb};
use oc_wasm_opencomputers::error::Error;
use oc_wasm_opencomputers::{gpu, keyboard, redstone, robot, screen};
use oc_wasm_safe::{component, computer};
use wee_alloc::WeeAlloc;

#[global_allocator]
static ALLOC: WeeAlloc<'_> = WeeAlloc::INIT;

const SCAFFOLD_SLOT: NonZeroU32 = unsafe { NonZeroU32::new_unchecked(1) };
const NORTH_Z: i32 = 372;
const SOUTH_Z: i32 = 396;
const EAST_X: i32 = -345;
const WEST_X: i32 = -350;
const BLOCKS_PER_LAYER: u32 = ((SOUTH_Z - NORTH_Z + 1) * (EAST_X - WEST_X + 1)) as u32;
const HOME_X: i32 = -342;
const HOME_Y: i32 = 65;
const HOME_Z: i32 = 377;
const HOME_DIR: Direction = Direction::East;

fn panic_hook(info: &PanicInfo<'_>) {
	if let Some(s) = info.payload().downcast_ref::<&str>() {
		computer::error(&format!("panic: {}", s));
	} else if let Some(s) = info.payload().downcast_ref::<String>() {
		computer::error(&format!("panic: {}", s));
	} else {
		computer::error("panic occurred");
	}
}

fn energy_percent() -> u8 {
	#[allow(clippy::cast_possible_truncation)] // Percent will always be ≤100.
	#[allow(clippy::cast_sign_loss)] // Percent will always be ≥0.
	let ret = (computer::energy().into_inner() * 100.0 / computer::max_energy().into_inner()) as u8;
	ret
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum Direction {
	North,
	South,
	East,
	West,
}

impl Direction {
	const fn dx(self) -> i32 {
		match self {
			Self::North | Self::South => 0,
			Self::East => 1,
			Self::West => -1,
		}
	}

	const fn dz(self) -> i32 {
		match self {
			Self::North => -1,
			Self::South => 1,
			Self::East | Self::West => 0,
		}
	}

	fn for_dx(dx: i32) -> Option<Self> {
		match dx.cmp(&0) {
			Ordering::Less => Some(Self::West),
			Ordering::Greater => Some(Self::East),
			Ordering::Equal => None,
		}
	}

	fn for_dz(dz: i32) -> Option<Self> {
		match dz.cmp(&0) {
			Ordering::Less => Some(Self::North),
			Ordering::Greater => Some(Self::South),
			Ordering::Equal => None,
		}
	}

	const fn turn(self, rot: robot::Rotation) -> Self {
		match rot {
			robot::Rotation::Counterclockwise => match self {
				Self::North => Self::West,
				Self::West => Self::South,
				Self::South => Self::East,
				Self::East => Self::North,
			},
			robot::Rotation::Clockwise => match self {
				Self::North => Self::East,
				Self::East => Self::South,
				Self::South => Self::West,
				Self::West => Self::North,
			},
		}
	}

	const fn rotation_towards(self, target: Self) -> Option<robot::Rotation> {
		type Rot = robot::Rotation;
		match self {
			Self::North => match target {
				Self::North => None,
				Self::South | Self::East => Some(Rot::Clockwise),
				Self::West => Some(Rot::Counterclockwise),
			},
			Self::South => match target {
				Self::North | Self::West => Some(Rot::Clockwise),
				Self::South => None,
				Self::East => Some(Rot::Counterclockwise),
			},
			Self::East => match target {
				Self::North => Some(Rot::Counterclockwise),
				Self::South | Self::West => Some(Rot::Clockwise),
				Self::East => None,
			},
			Self::West => match target {
				Self::North | Self::East => Some(Rot::Clockwise),
				Self::South => Some(Rot::Counterclockwise),
				Self::West => None,
			},
		}
	}

	const fn mirror(self) -> Self {
		match self {
			Self::North => Self::South,
			Self::South => Self::North,
			Self::East => Self::West,
			Self::West => Self::East,
		}
	}
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Navigator {
	x: i32,
	y: i32,
	z: i32,
	direction: Direction,
}

impl Navigator {
	async fn turn(
		&mut self,
		bot: &mut robot::Locked<'_, '_, Vec<u8>>,
		rotation: robot::Rotation,
	) -> Result<(), Error> {
		bot.turn(rotation).await?;
		self.direction = self.direction.turn(rotation);
		Ok(())
	}

	async fn turn_to(
		&mut self,
		bot: &mut robot::Locked<'_, '_, Vec<u8>>,
		direction: Direction,
	) -> Result<(), Error> {
		while let Some(rot) = self.direction.rotation_towards(direction) {
			self.turn(bot, rot).await?;
		}
		Ok(())
	}

	async fn move_robot(
		&mut self,
		bot: &mut robot::Locked<'_, '_, Vec<u8>>,
		direction: robot::MoveDirection,
	) -> Result<(), Error> {
		bot.move_robot(direction).await?;
		match direction {
			robot::MoveDirection::Down => self.y -= 1,
			robot::MoveDirection::Up => self.y += 1,
			robot::MoveDirection::Front => {
				self.x += self.direction.dx();
				self.z += self.direction.dz();
			}
			robot::MoveDirection::Back => {
				let dir = self.direction.mirror();
				self.x += dir.dx();
				self.z += dir.dz();
			}
		}
		Ok(())
	}

	async fn move_to(
		&mut self,
		bot: &mut robot::Locked<'_, '_, Vec<u8>>,
		x: i32,
		y: i32,
		z: i32,
	) -> Result<(), Error> {
		if let Some(dir) = Direction::for_dx(x - self.x) {
			self.turn_to(bot, dir).await?;
			while self.x != x {
				self.move_robot(bot, robot::MoveDirection::Front).await?;
			}
		}
		if let Some(dir) = Direction::for_dz(z - self.z) {
			self.turn_to(bot, dir).await?;
			while self.z != z {
				self.move_robot(bot, robot::MoveDirection::Front).await?;
			}
		}
		while self.y < y {
			self.move_robot(bot, robot::MoveDirection::Up).await?;
		}
		while self.y > y {
			self.move_robot(bot, robot::MoveDirection::Down).await?;
		}
		Ok(())
	}
}

async fn get_key(buffer: &mut Vec<u8>) -> Result<keyboard::BasicKeySignal, Error> {
	Ok(loop {
		sleep::until_signal().await;
		if let Some(len) = computer::pull_signal_length() {
			buffer.resize(len.get(), 0);
			let signal = computer::pull_signal(buffer)?.unwrap();
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

async fn read_line(
	invoker: &mut component::Invoker,
	buffer: &mut Vec<u8>,
	gpu: &gpu::Gpu,
	line: u32,
	screen_width: u32,
) -> Result<String, Error> {
	// Clear the prompt line and print the prompt and cursor.
	let mut gpu_locked = gpu.lock(invoker, buffer);
	gpu_locked
		.fill(
			Point { x: 1, y: line },
			Dimension {
				width: screen_width,
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

	// Record the string typed so far and the column number of the cursor.
	let mut data = String::new();
	let mut cursor_col = 3;

	// Handle keys.
	loop {
		let params = get_key(buffer).await?;
		if let Some(ch) = params.character {
			if ch == '\u{8}' {
				// This is a backspace. If there are any characters in the typed string, remove the
				// last one, move the cursor accordingly, and reprint the cursor, overwriting the
				// deleted character.
				if let Some(last) = data.pop() {
					cursor_col -= computer::char_width(last);
					gpu.lock(invoker, buffer)
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
				// This is another character. Store and print it and update the cursor position.
				data.push(ch);
				gpu.lock(invoker, buffer)
					.set(
						Point {
							x: cursor_col,
							y: line,
						},
						&format!("{}\u{2588}", ch),
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

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct PositionSweepWorking {
	init_x: i32,
	x: i32,
	z: i32,
}

impl PositionSweepWorking {
	fn advance(&self) -> PositionSweep {
		let is_east = self.init_x == WEST_X;
		let dx = (if is_east {
			Direction::East
		} else {
			Direction::West
		})
		.dx();
		let dest_x = if is_east { EAST_X } else { WEST_X };
		let current_column_is_north = (self.x - self.init_x) % 2 != 0;
		let dest_z = if current_column_is_north {
			NORTH_Z
		} else {
			SOUTH_Z
		};
		let dz = (if current_column_is_north {
			Direction::North
		} else {
			Direction::South
		})
		.dz();
		if self.z == dest_z {
			if self.x == dest_x {
				// Done.
				PositionSweep::Done
			} else {
				// Move to the next column.
				PositionSweep::Working(Self {
					x: self.x + dx,
					..*self
				})
			}
		} else {
			// Move along the column.
			PositionSweep::Working(Self {
				z: self.z + dz,
				..*self
			})
		}
	}
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum PositionSweep {
	Working(PositionSweepWorking),
	Done,
}

impl PositionSweep {
	fn new(x: i32, z: i32) -> Self {
		Self::Working(PositionSweepWorking { init_x: x, x, z })
	}
}

impl Iterator for PositionSweep {
	type Item = (i32, i32);

	fn next(&mut self) -> Option<Self::Item> {
		if let Self::Working(psw) = self {
			let ret = (psw.x, psw.z);
			*self = psw.advance();
			Some(ret)
		} else {
			None
		}
	}
}

impl core::iter::FusedIterator for PositionSweep {}

struct Application {
	invoker: component::Invoker,
	buffer: Vec<u8>,
	gpu: gpu::Gpu,
	screen: screen::Screen,
	robot: robot::Robot,
	redstone: redstone::Redstone,
}

impl Application {
	fn new() -> Self {
		let mut lister = component::Lister::take().unwrap();
		let invoker = component::Invoker::take().unwrap();
		let buffer = vec![];
		let gpu = gpu::Gpu::new(*lister.start(Some(gpu::TYPE)).next().unwrap().address());
		let screen =
			screen::Screen::new(*lister.start(Some(screen::TYPE)).next().unwrap().address());
		let robot = robot::Robot::new(*lister.start(Some(robot::TYPE)).next().unwrap().address());
		let redstone =
			redstone::Redstone::new(*lister.start(Some(redstone::TYPE)).next().unwrap().address());
		Self {
			invoker,
			buffer,
			gpu,
			screen,
			robot,
			redstone,
		}
	}

	async fn run(&mut self) -> Result<Infallible, Error> {
		// Initialize GPU and screen.
		let mut gpu_locked = self.gpu.lock(&mut self.invoker, &mut self.buffer);
		gpu_locked.bind(*self.screen.address(), true).await?;
		let screen_size = gpu_locked.max_resolution().await?;
		gpu_locked.set_resolution(screen_size).await?;
		gpu_locked.set_background(gpu::Colour::Rgb(Rgb(0))).await?;
		gpu_locked
			.set_foreground(gpu::Colour::Rgb(Rgb(0xFF_FF_FF)))
			.await?;

		// Check for a scaffold for comparison.
		let mut robot_locked = self.robot.lock(&mut self.invoker, &mut self.buffer);
		assert_ne!(
			robot_locked.count(SCAFFOLD_SLOT).await?,
			0,
			"Put a wooden scaffold in the first inventory slot"
		);

		// Check that all other slots are empty.
		let inventory_size = robot_locked.inventory_size().await?;
		for i in 2..=inventory_size {
			assert_eq!(
				robot_locked.count(NonZeroU32::new(i).unwrap()).await?,
				0,
				"All inventory slots other than the first should be empty"
			);
		}

		// Ask how many layers to fill.
		let mut gpu_locked = self.gpu.lock(&mut self.invoker, &mut self.buffer);
		gpu_locked
			.fill(Point { x: 1, y: 1 }, screen_size, ' ')
			.await?;
		gpu_locked
			.set(
				Point { x: 1, y: 1 },
				"How many layers?",
				gpu::TextDirection::Horizontal,
			)
			.await?;
		let layers = loop {
			let line = read_line(
				&mut self.invoker,
				&mut self.buffer,
				&self.gpu,
				3,
				screen_size.width,
			)
			.await?;
			if let Ok(n) = line.parse::<u32>() {
				if n > 0 {
					break n;
				}
			}
		};

		// Confirm that the robot is properly positioned.
		let mut gpu_locked = self.gpu.lock(&mut self.invoker, &mut self.buffer);
		gpu_locked
			.fill(Point { x: 1, y: 1 }, screen_size, ' ')
			.await?;
		for (i, text) in [
			"Is the robot adjacent to the charger",
			"and cobblestone chest, facing the former? [Y/N]",
		]
		.iter()
		.enumerate()
		{
			#[allow(clippy::cast_possible_truncation)] // Text line numbers are always small.
			gpu_locked
				.set(
					Point {
						x: 1,
						y: i as u32 + 1,
					},
					text,
					gpu::TextDirection::Horizontal,
				)
				.await?;
		}
		let properly_positioned = loop {
			let params = get_key(&mut self.buffer).await?;
			if let Some(ch) = params.character {
				if ch == 'y' || ch == 'Y' {
					break true;
				} else if ch == 'n' || ch == 'N' {
					break false;
				}
			}
		};
		assert!(properly_positioned, "Please reposition the robot");

		// Create the navigator.
		let mut nav = Navigator {
			x: HOME_X,
			y: HOME_Y,
			z: HOME_Z,
			direction: HOME_DIR,
		};

		// Do work.
		self.do_work(&mut nav, layers).await?;

		// do_work ends at home, so shut down.
		computer::shutdown();
	}

	async fn do_work(&mut self, nav: &mut Navigator, mut layers: u32) -> Result<(), Error> {
		// Calculate how many layers can be done per trip.
		let mut robot_locked = self.robot.lock(&mut self.invoker, &mut self.buffer);
		let inventory_size = robot_locked.inventory_size().await?;
		let layers_per_trip = (inventory_size * 64) / BLOCKS_PER_LAYER;

		while layers != 0 {
			// If we are low on energy, recharge.
			if energy_percent() < 95 {
				let mut rs_locked = self.redstone.lock(&mut self.invoker, &mut self.buffer);
				rs_locked
					.set_output(&[Some(15), Some(15), Some(15), Some(15), Some(15), Some(15)])
					.await?;
				while energy_percent() < 95 {
					sleep::for_uptime(Duration::from_secs(10)).await;
				}
				rs_locked
					.set_output(&[Some(0), Some(0), Some(0), Some(0), Some(0), Some(0)])
					.await?;
			}

			// Calculate how many layers we can fill on a single trip.
			let trip_layers = core::cmp::min(layers, layers_per_trip);
			layers -= trip_layers;

			// Load up on cobblestone.
			let mut robot_locked = self.robot.lock(&mut self.invoker, &mut self.buffer);
			robot_locked.select(NonZeroU32::new(2).unwrap()).await?;
			nav.turn_to(&mut robot_locked, Direction::North).await?;
			{
				let mut items_needed = trip_layers * BLOCKS_PER_LAYER;
				while items_needed != 0 {
					let items_sucked = robot_locked
						.suck(robot::ActionSide::Front, items_needed)
						.await
						.expect("Ran out of cobblestone in the chest");
					items_needed -= items_sucked;
				}
			}

			// Scan inventory slot counts.
			let mut inventory_counts = vec![];
			inventory_counts.reserve(inventory_size as usize);
			for i in 0..inventory_size {
				inventory_counts.push(robot_locked.count(NonZeroU32::new(i + 1).unwrap()).await?);
			}

			// Keep a record of the selected slot number.
			let mut selected_slot = robot_locked.selected().await?;

			// Move out over the hole.
			nav.move_to(&mut robot_locked, EAST_X, HOME_Y, HOME_Z)
				.await?;

			// Descend until we hit the floor.
			loop {
				let ret = nav
					.move_robot(&mut robot_locked, robot::MoveDirection::Down)
					.await;
				if let Err(e) = ret {
					if e == Error::Blocked(robot::BlockContent::Solid) {
						break;
					}
					panic!("Unexpected error {:?}", e);
				}
			}

			// Climb back up one level.
			nav.move_robot(&mut robot_locked, robot::MoveDirection::Up)
				.await?;

			// Move to the northeast corner.
			nav.move_to(&mut robot_locked, EAST_X, nav.y, NORTH_Z)
				.await?;

			// Do the layers.
			for _ in 0..trip_layers {
				// Fill the layer.
				for (x, z) in PositionSweep::new(nav.x, nav.z) {
					// Move to the position.
					if nav.move_to(&mut robot_locked, x, nav.y, z).await.is_err() {
						// Move failed; the scaffold might be in the way. Try breaking it and
						// moving again.
						robot_locked.select(SCAFFOLD_SLOT).await?;
						selected_slot = SCAFFOLD_SLOT;
						robot_locked
							.swing(robot::ActionSide::Front, None, false)
							.await?;
						inventory_counts[selected_slot.get() as usize - 1] += 1;
						nav.move_to(&mut robot_locked, x, nav.y, z).await?;
					}

					// Find the first slot that has cobblestone in it, and place it down.
					for slot in 2..=inventory_size {
						let slot = NonZeroU32::new(slot).unwrap();
						if inventory_counts[slot.get() as usize - 1] != 0 {
							if slot != selected_slot {
								robot_locked.select(slot).await?;
								selected_slot = slot;
							}
							robot_locked
								.place(robot::ActionSide::Bottom, None, false)
								.await?;
							inventory_counts[selected_slot.get() as usize - 1] -= 1;
							break;
						}
					}
				}

				// Move up for the next layer.
				nav.move_robot(&mut robot_locked, robot::MoveDirection::Up)
					.await?;
			}

			// Go home.
			let mut robot_locked = self.robot.lock(&mut self.invoker, &mut self.buffer);
			nav.move_to(&mut robot_locked, EAST_X, nav.y, HOME_Z)
				.await?;
			nav.move_to(&mut robot_locked, EAST_X, HOME_Y, HOME_Z)
				.await?;
			nav.move_to(&mut robot_locked, HOME_X, HOME_Y, HOME_Z)
				.await?;
			nav.turn_to(&mut robot_locked, HOME_DIR).await?;
		}
		Ok(())
	}
}

async fn main_impl() -> Result<Infallible, Error> {
	Application::new().run().await
}

async fn main() -> Infallible {
	// Set the panic hook.
	std::panic::set_hook(Box::new(panic_hook));

	// Run the main function and report errors if it returns one.
	match main_impl().await {
		Ok(i) => i,
		Err(e) => computer::error(&format!("main_impl returned {}", e.as_str())),
	}
}

#[no_mangle]
pub extern "C" fn run(arg: i32) -> i32 {
	oc_wasm_cassette::run(arg, main)
}
