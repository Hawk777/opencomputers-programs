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
use core::panic::PanicInfo;
use core::pin::Pin;
use minicbor::decode::Decoder;
use oc_wasm_futures::sleep;
use oc_wasm_opencomputers::{
	common::{Dimension, Point, Rgb},
	filesystem, gpu, keyboard, screen,
};
use oc_wasm_safe::{component, computer, execute};
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

async fn read_line(
	invoker: &mut component::Invoker,
	buffer: &mut Vec<u8>,
	gpu: &gpu::Gpu,
	line: u32,
	screen_width: u32,
) -> Result<String, oc_wasm_opencomputers::error::Error> {
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
		sleep::until_signal().await;
		if let Some(len) = computer::pull_signal_length() {
			// Pop and handle signal.
			buffer.resize(len.get(), 0);
			let signal = computer::pull_signal(buffer)?.unwrap();
			let mut decoder = Decoder::new(&signal);
			let signal_name = decoder
				.str()
				.map_err(|_| oc_wasm_safe::error::Error::CborDecode)?;
			if signal_name == keyboard::KeySignal::KEY_DOWN {
				let params: keyboard::KeySignal<'_> = decoder
					.decode()
					.map_err(|_| oc_wasm_safe::error::Error::CborDecode)?;
				let params = params.to_basic();
				if let Some(ch) = params.character {
					if ch == '\u{8}' {
						// This is a backspace. If there are any characters in the typed string,
						// remove the last one, move the cursor accordingly, and reprint the
						// cursor, overwriting the deleted character.
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
						// This is another character. Store and print it and update the cursor
						// position.
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
		}
	}

	// Return the typed line.
	Ok(data)
}

fn filename_is_wasm(filename: &str) -> bool {
	if filename.len() >= 5 {
		if let Some(last_five) = filename.get((filename.len() - 5)..) {
			if last_five.eq_ignore_ascii_case(".wasm") {
				return true;
			}
		}
	}
	false
}

async fn main_impl() -> Result<Infallible, oc_wasm_opencomputers::error::Error> {
	// Grab objects.
	let mut lister = component::Lister::take().unwrap();
	let mut invoker = component::Invoker::take().unwrap();
	let mut buffer = vec![];

	// Initialize GPU and screen.
	let gpu = gpu::Gpu::new(*lister.start(Some(&gpu::TYPE)).next().unwrap().address());
	let screen = screen::Screen::new(*lister.start(Some(&screen::TYPE)).next().unwrap().address());
	let mut gpu_locked = gpu.lock(&mut invoker, &mut buffer);
	gpu_locked.bind(*screen.address(), true).await?;
	let screen_size = gpu_locked.max_resolution().await?;
	gpu_locked.set_resolution(screen_size).await?;
	gpu_locked.set_background(gpu::Colour::Rgb(Rgb(0))).await?;
	gpu_locked
		.set_foreground(gpu::Colour::Rgb(Rgb(0xFF_FF_FF)))
		.await?;

	// Find filesystem components.
	let filesystems = {
		let mut addresses = vec![];
		let mut list = lister.start(Some(&filesystem::TYPE));
		while let Some(item) = list.next() {
			addresses.push(*item.address());
		}
		addresses
	};

	// Let the user pick one.
	let mut gpu_locked = gpu.lock(&mut invoker, &mut buffer);
	gpu_locked
		.fill(Point { x: 1, y: 1 }, screen_size, ' ')
		.await?;
	for (index, address) in filesystems.iter().enumerate() {
		#[allow(clippy::cast_possible_truncation)]
		// The number of files needs to fit on the screen anyway.
		gpu_locked
			.set(
				Point {
					x: 1,
					y: (index + 1) as u32,
				},
				&format!("{} {}", index + 1, address),
				gpu::TextDirection::Horizontal,
			)
			.await?;
	}
	let filesystem = filesystem::Filesystem::new(loop {
		#[allow(clippy::cast_possible_truncation)]
		// The number of files needs to fit on the screen anyway.
		let line = read_line(
			&mut invoker,
			&mut buffer,
			&gpu,
			(filesystems.len() + 2) as u32,
			screen_size.width,
		)
		.await?;
		if let Ok(index) = line.parse::<usize>() {
			if 1 <= index && index <= filesystems.len() {
				break filesystems[index - 1];
			}
		}
	});
	drop(filesystems);

	// Let the user pick a file from that filesystem.
	let mut files_buffer = vec![];
	let files: Vec<&str> = filesystem
		.lock(&mut invoker, &mut files_buffer)
		.list("")
		.await?
		.into_iter()
		.filter(|e| {
			e.object_type == filesystem::DirectoryEntryType::File && filename_is_wasm(e.name)
		})
		.map(|e| e.name)
		.collect();
	let mut gpu_locked = gpu.lock(&mut invoker, &mut buffer);
	gpu_locked
		.fill(Point { x: 1, y: 1 }, screen_size, ' ')
		.await?;
	for (index, file) in files.iter().enumerate() {
		#[allow(clippy::cast_possible_truncation)]
		// The number of files needs to fit on the screen anyway.
		gpu_locked
			.set(
				Point {
					x: 1,
					y: (index + 1) as u32,
				},
				&format!("{} {}", index + 1, file),
				gpu::TextDirection::Horizontal,
			)
			.await?;
	}
	let file = loop {
		#[allow(clippy::cast_possible_truncation)]
		// The number of files needs to fit on the screen anyway.
		let line = read_line(
			&mut invoker,
			&mut buffer,
			&gpu,
			(files.len() + 2) as u32,
			screen_size.width,
		)
		.await?;
		if let Ok(index) = line.parse::<usize>() {
			if 1 <= index && index <= files.len() {
				break files[index - 1];
			}
		}
	};
	drop(files);

	// Open, read, and execute that file.
	execute::clear();
	let handle = filesystem
		.lock(&mut invoker, &mut buffer)
		.open_read(file)
		.await?;
	while let Some(bytes_read) = handle.lock(&mut invoker, &mut buffer).read(4096).await? {
		execute::add(bytes_read)?;
	}
	drop(handle);
	execute::execute();
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
