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

use core::convert::Infallible;
use core::panic::PanicInfo;
use core::time::Duration;
use oc_wasm_futures::sleep;
use oc_wasm_opencomputers::common::Point;
use oc_wasm_opencomputers::prelude::*;
use oc_wasm_opencomputers::{gpu, screen};
use oc_wasm_safe::{component, computer};

#[global_allocator]
static ALLOC: lol_alloc::AssumeSingleThreaded<lol_alloc::LeakingAllocator> =
	unsafe { lol_alloc::AssumeSingleThreaded::new(lol_alloc::LeakingAllocator::new()) };

fn panic_hook(info: &PanicInfo<'_>) {
	if let Some(s) = info.payload().downcast_ref::<&str>() {
		computer::error(&format!("panic: {}", s));
	} else if let Some(s) = info.payload().downcast_ref::<String>() {
		computer::error(&format!("panic: {}", s));
	} else {
		computer::error("panic occurred");
	}
}

async fn main_impl() -> Result<Infallible, oc_wasm_opencomputers::error::Error> {
	// Grab resources.
	let mut lister = component::Lister::take().unwrap();
	let mut invoker = component::Invoker::take().unwrap();
	let mut buffer = vec![];
	buffer.reserve(4096);

	// Find the GPU and screen.
	let gpu = gpu::Gpu::new(*lister.start(Some(gpu::TYPE)).next().unwrap().address());
	let screen = screen::Screen::new(*lister.start(Some(screen::TYPE)).next().unwrap().address());

	// Bind and clear the screen.
	let mut gpu_locked = gpu.lock(&mut invoker, &mut buffer);
	gpu_locked.bind(*screen.address(), true).await?;
	let screen_size = gpu_locked.max_resolution().await?;
	gpu_locked.set_resolution(screen_size).await?;
	gpu_locked
		.fill(Point { x: 1, y: 1 }, screen_size, ' ')
		.await?;

	// Display a list of components.
	let mut list = lister.start(None);
	let mut row = 1;
	let mut buffer2 = vec![];
	while let Some(entry) = list.next() {
		buffer2.resize(entry.type_name_len().get(), 0);
		let type_name = entry.type_name(&mut buffer2)?;
		gpu_locked
			.set(
				Point { x: 1, y: row },
				type_name,
				gpu::TextDirection::Horizontal,
			)
			.await?;
		row += 1;
	}

	// Sleep forever.
	loop {
		sleep::for_uptime(Duration::from_secs(3600)).await;
	}
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
