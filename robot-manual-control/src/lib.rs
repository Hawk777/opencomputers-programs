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

use cassette::Cassette;
use core::convert::Infallible;
use core::future::Future;
use core::num::NonZeroU32;
use core::panic::PanicInfo;
use core::pin::Pin;
use minicbor::decode::Decoder;
use oc_wasm_futures::sleep;
use oc_wasm_opencomputers::common::{Point, Rgb};
use oc_wasm_opencomputers::{gpu, keyboard, robot, screen};
use oc_wasm_safe::{component, computer};
use once_cell::unsync::OnceCell;
use wee_alloc::WeeAlloc;

#[global_allocator]
static ALLOC: WeeAlloc<'_> = WeeAlloc::INIT;

fn panic_hook(info: &PanicInfo<'_>) {
	if let Some(s) = info.payload().downcast_ref::<&str>() {
		computer::error(&format!("panic: {}", s));
	} else if let Some(s) = info.payload().downcast_ref::<String>() {
		computer::error(&format!("panic: {}", s));
	} else {
		computer::error("panic occurred");
	}
}

struct Application {
	invoker: component::Invoker,
	buffer: Vec<u8>,
	gpu: gpu::Gpu,
	screen: screen::Screen,
	robot: robot::Robot,
}

impl Application {
	async fn new() -> Result<Application, oc_wasm_opencomputers::error::Error> {
		let mut lister = component::Lister::take().unwrap();
		let invoker = component::Invoker::take().unwrap();
		let buffer = vec![];
		let gpu = gpu::Gpu::new(*lister.start(Some(&gpu::TYPE)).next().unwrap().address());
		let screen =
			screen::Screen::new(*lister.start(Some(&screen::TYPE)).next().unwrap().address());
		let robot = robot::Robot::new(*lister.start(Some(&robot::TYPE)).next().unwrap().address());
		Ok(Application {
			invoker,
			buffer,
			gpu,
			screen,
			robot,
		})
	}

	async fn run(&mut self) -> Result<Infallible, oc_wasm_opencomputers::error::Error> {
		// Initialize GPU and screen.
		let mut gpu_locked = self.gpu.lock(&mut self.invoker, &mut self.buffer);
		gpu_locked.bind(*self.screen.address(), true).await?;
		let screen_size = gpu_locked.max_resolution().await?;
		gpu_locked.set_resolution(screen_size).await?;
		gpu_locked.set_background(gpu::Colour::Rgb(Rgb(0))).await?;
		gpu_locked
			.set_foreground(gpu::Colour::Rgb(Rgb(0xFF_FF_FF)))
			.await?;

		loop {
			// Display main menu.
			const HELP_LINES: [Option<&str>; 10] = [
				Some("Robot Manual Control Main Menu"),
				None,
				None,
				None,
				Some("W/S: Move Forward/Back"),
				Some("A/D: Turn Left/Right"),
				Some("Q/E: Move Down/Up"),
				Some("I: Select Inventory Slot"),
				Some("R: Reboot"),
				Some("P: Power Down"),
			];
			let mut gpu_locked = self.gpu.lock(&mut self.invoker, &mut self.buffer);
			gpu_locked
				.fill(Point { x: 1, y: 1 }, screen_size, ' ')
				.await?;
			for (index, line) in HELP_LINES.iter().enumerate() {
				if let Some(line) = line {
					#[allow(clippy::cast_possible_truncation)] // Line numbers are not that big.
					gpu_locked
						.set(
							Point {
								x: 1,
								y: (index + 1) as u32,
							},
							line,
							gpu::TextDirection::Horizontal,
						)
						.await?;
				}
			}

			loop {
				// Display live energy level.
				let mut gpu_locked = self.gpu.lock(&mut self.invoker, &mut self.buffer);
				#[allow(clippy::cast_possible_truncation)] // Energy levels are not that big.
				gpu_locked
					.set(
						Point { x: 1, y: 3 },
						&format!(
							"Energy: {}%   ",
							computer::energy().into_inner() as i64 * 100
								/ computer::max_energy().into_inner() as i64
						),
						gpu::TextDirection::Horizontal,
					)
					.await?;

				// Handle any signals.
				sleep::until_signal().await;
				while let Some(len) = computer::pull_signal_length() {
					// Pop and handle signal.
					self.buffer.resize(len.get(), 0);
					let signal = computer::pull_signal(&mut self.buffer)?.unwrap();
					let mut decoder = Decoder::new(&signal);
					let signal_name = decoder
						.str()
						.map_err(|_| oc_wasm_safe::error::Error::CborDecode)?;
					if signal_name == keyboard::KeySignal::KEY_DOWN {
						let data: keyboard::KeySignal<'_> = decoder
							.decode()
							.map_err(|_| oc_wasm_safe::error::Error::CborDecode)?;
						let data = data.to_basic();
						if let Some(ch) = data.character {
							let mut robot_locked =
								self.robot.lock(&mut self.invoker, &mut self.buffer);
							if ch == 'w' || ch == 'W' {
								robot_locked.move_robot(robot::MoveDirection::Front).await?;
							} else if ch == 's' || ch == 'S' {
								robot_locked.move_robot(robot::MoveDirection::Back).await?;
							} else if ch == 'a' || ch == 'A' {
								robot_locked.turn(robot::Rotation::Counterclockwise).await?;
							} else if ch == 'd' || ch == 'D' {
								robot_locked.turn(robot::Rotation::Clockwise).await?;
							} else if ch == 'q' || ch == 'Q' {
								robot_locked.move_robot(robot::MoveDirection::Down).await?;
							} else if ch == 'e' || ch == 'E' {
								robot_locked.move_robot(robot::MoveDirection::Up).await?;
							} else if ch == 'i' || ch == 'I' {
								let slot = robot_locked.selected().await?;
								let size = robot_locked.inventory_size().await?;
								let slot = NonZeroU32::new(if slot.get() == size {
									1
								} else {
									slot.get() + 1
								})
								.unwrap();
								robot_locked.select(slot).await?;
							} else if ch == 'r' || ch == 'R' {
								computer::reboot();
							} else if ch == 'p' || ch == 'P' {
								computer::shutdown();
							}
						}
					}
				}
			}
		}
	}
}

async fn main_impl() -> Result<Infallible, oc_wasm_opencomputers::error::Error> {
	Application::new().await?.run().await
}

async fn main() -> Infallible {
	match main_impl().await {
		Ok(i) => i,
		Err(e) => computer::error(&format!("main_impl returned {}", e.as_str())),
	}
}

#[no_mangle]
pub extern "C" fn run(_: i32) -> i32 {
	static mut PANIC_HOOK_SET: bool = false;
	static mut EXECUTOR: OnceCell<Cassette<Pin<Box<dyn Future<Output = Infallible>>>>> =
		OnceCell::new();

	// SAFETY: run() is not reentrant and never touches the PANIC_HOOK_SET variable anywhere else
	// in its body, so run() will never create a second mutable reference. PANIC_HOOK_SET is local
	// to run(), so nobody else can create a second mutable reference on the same thread. OC-Wasm
	// is single-threaded, so no other threads can call run() at the same time.
	let panic_hook_set = unsafe { &mut PANIC_HOOK_SET };
	if !*panic_hook_set {
		std::panic::set_hook(Box::new(panic_hook));
		*panic_hook_set = true;
	}

	// SAFETY: run() is not reentrant and never touches the EXECUTOR variable anywhere else in its
	// body, so run() will never create a second mutable reference. EXECUTOR is local to run(), so
	// nobody else can create a second mutable reference on the same thread. OC-Wasm is
	// single-threaded, so no other threads can call run() at the same time.
	let executor = unsafe { &mut EXECUTOR };
	executor.get_or_init(|| Cassette::new(Box::pin(main())));
	let executor = executor.get_mut().unwrap_or_else(
		// SAFETY: We just called get_or_init(), so it must be populated.
		|| panic!("executor is missing"),
	);

	sleep::check_for_wakeups();
	if executor.poll_on().is_some() {
		computer::error("main task terminated");
	}
	sleep::shortest_requested()
}
