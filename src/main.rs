#![allow(unused_variables)]
#![allow(unused_imports)]
#![allow(dead_code)]

extern crate glfw;
extern crate gl;
extern crate nalgebra;
extern crate rodio;

mod board;
mod renderer;

use self::glfw::{ Context, Key, Action };
use nalgebra::{ Matrix4, Vector4, Vector3 };
use rodio::{ source::SamplesConverter, Decoder, OutputStream, OutputStreamHandle, source::Source };
use std::fs::File;
use std::io::BufReader;

use board::*;
use renderer::*;

// Settings
const WINDOW_WIDTH: u32 = 640;
const WINDOW_HEIGHT: u32 = 480;
const WINDOW_TITLE: &'static str = "Hello, world";

//#[derive(Debug)]
struct GameInfo
{
	last_time: f64,
	board: Board,
	board_renderer: BoardRenderer,
	window_size: (u32, u32),
	board_scale: f64,
	hover: Option<Pos>,
	from_piece: Option<Piece>,
	stream: OutputStreamHandle
}

impl GameInfo
{
	fn update(&self, _delta_time: f64)
	{

	}

	fn render(&mut self)
	{
		unsafe
		{
			gl::ClearColor(0.16, 0.17, 0.2, 1.0);
			gl::Clear(gl::COLOR_BUFFER_BIT);
		}

		let from_color = Vector4::new(0.68, 0.92, 0.63, 0.5);
		let to_color = Vector4::new(0.83, 0.35, 0.07, 0.5);
		let check_color = Vector4::new(1.0, 0.0, 0.0, 0.5);
		self.board_renderer.render(&self.board, self.board_scale as f32);

		match self.from_piece
		{
			Some(piece) =>
			{
				self.board_renderer.render_square(self.board_scale as f32, piece.pos(), from_color);

				if let Some(hover) = self.hover
				{
					self.board_renderer.render_square(self.board_scale as f32, hover, to_color);
				}
			},
			None =>
			{
				if let Some(hover) = self.hover
				{
					self.board_renderer.render_square(self.board_scale as f32, hover, from_color);
				}
			}
		}

		if self.board.is_check(Color::White)
		{
			self.board_renderer.render_square(self.board_scale as f32, self.board.find_pieces(Some(PieceType::King), Some(Color::White), None).first().unwrap().pos(), check_color);
		}

		if self.board.is_check(Color::Black)
		{
			self.board_renderer.render_square(self.board_scale as f32, self.board.find_pieces(Some(PieceType::King), Some(Color::Black), None).first().unwrap().pos(), check_color);
		}
	}
}

extern "system"
fn debug_callback(source: gl::types::GLenum, gltype: gl::types::GLenum, id: gl::types::GLuint, severity: gl::types::GLenum, length: gl::types::GLsizei, message: *const gl::types::GLchar, user_param: *mut std::ffi::c_void)
{
	let message = unsafe { std::ffi::CStr::from_ptr(message).to_str().unwrap() };
	println!("Debug callback! Message:\n{}", message);
}

fn main() {
    println!("Hello, world!");

	let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS).expect("Failed to initialize glfw");
	glfw.window_hint(glfw::WindowHint::ContextVersion(3, 3));
	glfw.window_hint(glfw::WindowHint::OpenGlProfile(glfw::OpenGlProfileHint::Core));

	#[cfg(target_os = "macos")]
	glfw.window_hint(glfw::WindowHint::OpenGlForwardCompat(true));

	let (mut window, _events) = glfw.create_window(WINDOW_WIDTH, WINDOW_HEIGHT, WINDOW_TITLE, glfw::WindowMode::Windowed).expect("Failed to create glfw window");

	window.make_current();
	window.set_key_polling(true);
	window.set_framebuffer_size_polling(true);
	window.set_cursor_pos_polling(true);
	window.set_mouse_button_polling(true);
	glfw.set_swap_interval(glfw::SwapInterval::Sync(1));

	gl::load_with(|symbol| window.get_proc_address(symbol) as *const std::ffi::c_void);

	unsafe
	{
		gl::Enable(gl::DEBUG_OUTPUT);
		gl::DebugMessageCallback(Some(debug_callback), std::ptr::null());
	}

	unsafe
	{
		//gl::Disable(gl::DEPTH_TEST);
		gl::Enable(gl::BLEND);
		gl::BlendFunc(gl::SRC_ALPHA, gl::ONE_MINUS_SRC_ALPHA);
	}

	let board = Board::standard();

	let (_stream, stream_handle) = OutputStream::try_default().expect("Could not find an audio device");

	let mut info = GameInfo {
		last_time: glfw.get_time(),
		board,
		board_renderer: BoardRenderer::new(WINDOW_WIDTH as f32 / WINDOW_HEIGHT as f32),
		window_size: (WINDOW_WIDTH, WINDOW_HEIGHT),
		board_scale: 0.8,
		hover: None,
		from_piece: None,
		stream: stream_handle
	};

	while !window.should_close()
	{
		let now = glfw.get_time();
		let delta_time = now - info.last_time;
		info.last_time = now;

		/* Update */
		info.update(delta_time);

		/* Render */
		info.render();

		/* Swap buffers and poll events */
		window.swap_buffers();
		glfw.poll_events_unbuffered(|window_id: glfw::WindowId, (id, event): (f64, glfw::WindowEvent)|
		{
			if window_id == window.window_id()
			{
				process_event(&mut info, &mut window, &event);
			}

			return Some((id, event));
		});
	}
}

fn process_event(info: &mut GameInfo, window: &mut glfw::Window, event: &glfw::WindowEvent)
{
	match event
	{
		glfw::WindowEvent::FramebufferSize(width, height) =>
		{
			// Update the window size
			unsafe { gl::Viewport(0, 0, *width, *height); }
			info.board_renderer.set_aspect(*width as f32 / *height as f32);
			info.window_size = (*width as u32, *height as u32);

			let now = window.glfw.get_time();
			let delta_time = now - info.last_time;
			info.last_time = now;

			info.update(delta_time);
			info.render();

			window.swap_buffers();
		},
		glfw::WindowEvent::Key(Key::Escape, _, Action::Release, _) =>
		{
			window.set_should_close(true);
		},
		glfw::WindowEvent::CursorPos(x, y) =>
		{
			let board_size = info.window_size.0.min(info.window_size.1) as f64;
			let diff = (info.window_size.0.max(info.window_size.1) as f64 - board_size) / 2.0;
			let padding = (1.0 - info.board_scale) * board_size;

			let x = if info.window_size.0 > info.window_size.1 { x - diff } else { *x };
			let y = if info.window_size.1 > info.window_size.0 { y - diff } else { *y };

			let x = ((x - padding / 2.0) * 8.0 / (board_size - padding)).floor() as i32;
			let y = ((y - padding / 2.0) * 8.0 / (board_size - padding)).floor() as i32;
			
			if x >= 0 && x < 8 && y >= 0 && y < 8
			{
				info.hover = Some(Pos::new(x as u8, y as u8));
			}
			else
			{
				info.hover = None;
			}
		},
		glfw::WindowEvent::MouseButton(glfw::MouseButtonRight, Action::Release, _) =>
		{
			match info.hover
			{
				Some(hover) =>
				{
					println!("{} ({}, {}) = {}", hover, hover.x(), hover.y(), hover.value);
				},
				None => ()
			}
		}
		glfw::WindowEvent::MouseButton(glfw::MouseButtonLeft, Action::Release, _) =>
		{
			match info.hover
			{
				Some(hover) =>
				{
					match info.from_piece
					{
						Some(from_piece) =>
						{
							if from_piece.pos() != hover
							{
								// Check if move is legal
								if !info.board.is_legal_move(&from_piece, hover)
								{
									println!("Illegal move");
									info.from_piece = None;
									return;
								}

								// Check if can capture first
								/*
								if let Some(piece) = info.board.piece_at(hover)
								{
									if piece.color() == from_piece.color()
									{
										println!("Cannot capture piece {} at {} because it is the same color ({:?})", piece, piece.pos(), piece.color());
										info.from_piece = Some(piece);
										return;
									}
									else
									{
										let captured = info.board.remove_at(hover).unwrap();

										println!("Captured piece {} (at {})", captured, captured.pos());
									}
								}
								*/

								info.board.make_move(&from_piece, hover).expect(format!("Failed to make move {}{} (from {})", from_piece, hover, from_piece.pos()).as_str());
								info.board.active_color = from_piece.color().opposite();

								let source = Decoder::new(BufReader::new(File::open("sounds/move.wav").unwrap())).unwrap().convert_samples();
	
								info.stream.play_raw(source).unwrap();
								println!("Made move {}", move_notation(&from_piece, hover));
							}		

							info.from_piece = None;
							println!("from_piece = None");
						},
						None => {
							// Check if move is correct color

							match info.board.piece_at(hover)
							{
								Some(piece) =>
								{
									if piece.color() == info.board.active_color
									{
										info.from_piece = Some(piece);
										println!("from_piece = {} ({})", piece, hover);
									}
									else
									{
										println!("Invalid color move");
									}
								},
								None =>
								{
									info.from_piece = None;
									println!("from_piece = None");
								}
							}
						}
					}
				},
				None => ()
			}
		},
		_ => ()
	}
}
