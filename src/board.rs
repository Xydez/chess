use crate::renderer::*;
use crate::gl;

use nalgebra::{ Matrix4, Vector4, Vector3 };
use std::path::Path;
use std::collections::HashMap;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum PieceType
{
	Pawn, Rook, Knight, Bishop, Queen, King
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Color
{
	White, Black
}

#[derive(Debug, Copy, Clone)]
pub struct Piece
{
	piece_type: PieceType,
	color: Color,
	pos: u8
}

impl Piece
{
	pub fn new(color: Color, piece_type: PieceType, x: u8, y: u8) -> Piece
	{
		return Piece
		{
			piece_type, color,
			pos: (y * 8 + x)
		};
	}
}

#[derive(Debug)]
pub struct Board
{
	pieces: Vec<Piece>
}

impl Board
{
	pub fn empty() -> Board
	{
		return Board { pieces: Vec::new() };
	}

	pub fn piece_at(&self, x: u8, y: u8) -> Option<Piece>
	{
		let i = y * 8 + x;

		return self.piece_at_index(i);
	}

	pub fn piece_at_index(&self, i: u8) -> Option<Piece>
	{
		for piece in self.pieces.iter()
		{
			if piece.pos == i
			{
				return Some(*piece);
			}
		}

		return None;
	}

	pub fn add(&mut self, piece: Piece) -> Result<(), ()>
	{
		if self.piece_at_index(piece.pos).is_some()
		{
			return Err(());
		}
		else
		{
			self.pieces.push(piece);
			
			return Ok(());
		}
	}
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
struct PieceInfo
{
	color: Color,
	piece_type: PieceType
}

#[derive(Debug)]
struct PieceTextures
{
	texture_map: HashMap<(Color, PieceType), Texture>
}

impl PieceTextures
{
	fn new() -> PieceTextures
	{
		let mut texture_map = HashMap::new();

		texture_map.insert((Color::White, PieceType::King), Texture::load(Path::new("textures/lk.png")).unwrap());
		texture_map.insert((Color::Black, PieceType::King), Texture::load(Path::new("textures/dk.png")).unwrap());

		return PieceTextures { texture_map };
	}

	fn get(&self, piece: &Piece) -> Option<&Texture>
	{
		return self.texture_map.get(&(piece.color, piece.piece_type));
	}
}

#[derive(Debug)]
pub struct BoardRenderer
{
	mesh: Mesh,
	texture_mesh: Mesh,
	board_program: ShaderProgram,
	square_program: ShaderProgram,
	texture_program: ShaderProgram,
	piece_textures: PieceTextures,
	aspect: f32
}

impl Uniform for Vector3<f32>
{
	unsafe fn set_uniform(&self, id: gl::types::GLint)
	{
		gl!(gl::Uniform3f(id, self.x, self.y, self.z););
	}
}

impl Uniform for Vector4<f32>
{
	unsafe fn set_uniform(&self, id: gl::types::GLint)
	{
		gl!(gl::Uniform4f(id, self.x, self.y, self.z, self.w););
	}
}

impl Uniform for Matrix4<f32>
{
	unsafe fn set_uniform(&self, id: gl::types::GLint)
	{
		gl!(gl::UniformMatrix4fv(id, 1, gl::FALSE, self.as_ptr()););
	}
}

impl BoardRenderer
{
	pub fn new(aspect: f32) -> BoardRenderer
	{
		let fragment = ShaderSource::load("shaders/fragment.glsl").unwrap();

		let board_program = ShaderProgramBuilder::new()
			.vertex(ShaderSource::load("shaders/board_vertex.glsl").unwrap().compile(ShaderType::Vertex).unwrap())
			.fragment(fragment.clone().compile(ShaderType::Fragment).unwrap())
			.build().unwrap();

		let square_program = ShaderProgramBuilder::new()
			.vertex(ShaderSource::load("shaders/square_vertex.glsl").unwrap().compile(ShaderType::Vertex).unwrap())
			.fragment(fragment.compile(ShaderType::Fragment).unwrap())
			.build().unwrap();

		let texture_program = ShaderProgramBuilder::new()
			.vertex(ShaderSource::load("shaders/texture_vertex.glsl").unwrap().compile(ShaderType::Vertex).unwrap())
			.fragment(ShaderSource::load("shaders/texture_fragment.glsl").unwrap().compile(ShaderType::Fragment).unwrap())
			.build().unwrap();

		return BoardRenderer {
			mesh: Mesh::new(&vec![
				-1.00, -0.75, 0.0,
				-0.75, -0.75, 0.0,
				-0.75, -1.00, 0.0,
				-1.00, -1.00, 0.0
			],
			&vec![
				0, 1, 2,
				0, 2, 3
			]),
			texture_mesh: Mesh::with_layout(&vec![
				-1.00, -0.75, 0.0,   0.0, 0.0,
				-0.75, -0.75, 0.0,   1.0, 0.0,
				-0.75, -1.00, 0.0,   1.0, 1.0,
				-1.00, -1.00, 0.0,   0.0, 1.0
			],
			//texture_mesh: Mesh::with_layout(&vec![
			//	-1.0,  1.0, 0.0,   0.0, 1.0,
			//	 1.0,  1.0, 0.0,   1.0, 1.0,
			//	 1.0, -1.0, 0.0,   1.0, 0.0,
			//	-1.0, -1.0, 0.0,   0.0, 0.0
			//],
			&vec![
				0, 1, 2,
				0, 2, 3
			],
			&vec![(VertexBufferElement::Float, 3), (VertexBufferElement::Float, 2)]),
			board_program, square_program, texture_program,
			piece_textures: PieceTextures::new(),
			aspect
		};
	}

	pub fn set_aspect(&mut self, aspect: f32)
	{
		self.aspect = aspect;
	}

	pub fn render(&mut self, board: &Board, scale: f32, cursor: Option<u8>)
	{
		let mat_view = Matrix4::new_translation(&Vector3::<f32>::new(0.0, 0.0, -1.0));

		let w = self.aspect.max(1.0);
		let h = 1.0f32.max(1.0 / self.aspect);
		let mat_proj = Matrix4::new_orthographic(-w, w, -h, h, 0.1, 100.0);

		let view_proj = mat_proj * mat_view;

		self.render_board(&view_proj, scale);

		for piece in board.pieces.iter()
		{
			self.render_piece(&view_proj, scale, piece);
		}

		if cursor.is_some()
		{
			self.render_cursor(&view_proj, scale, cursor.unwrap());
		}
	}

	fn render_board(&mut self, view_proj: &Matrix4<f32>, scale: f32)
	{
		// #E2C5AF
		//self.program.set_uniform("green_color", Vector3::new(0.68, 0.92, 0.63)).unwrap();
		self.board_program.set_uniform("white_color", &Vector3::new(0.89, 0.77, 0.69)).unwrap();
		self.board_program.set_uniform("black_color", &Vector3::new(0.62, 0.43, 0.35)).unwrap();

		let mat_model = Matrix4::new_scaling(scale);

		self.board_program.set_uniform("mvp", &(view_proj * mat_model)).unwrap();
		render_elements_instanced(&self.board_program, &self.mesh.vao, &self.mesh.ibo, 64);
	}

	fn render_cursor(&mut self, view_proj: &Matrix4<f32>, scale: f32, pos: u8)
	{
		self.square_program.set_uniform("color", &Vector3::new(0.68, 0.92, 0.63)).unwrap();

		let x = pos % 8;
		let y = 7 - pos / 8;
		let mat_model = Matrix4::new_scaling(scale) * Matrix4::new_translation(&Vector3::<f32>::new(x as f32 * 0.25, y as f32 * 0.25, 0.0));

		self.square_program.set_uniform("mvp", &(view_proj * mat_model)).unwrap();
		render_elements(&self.square_program, &self.mesh.vao, &self.mesh.ibo);
	}

	fn render_piece(&mut self, view_proj: &Matrix4<f32>, scale: f32, piece: &Piece)
	{
		self.texture_program.set_uniform("u_texture_in", self.piece_textures.get(piece).expect("Piece texture not implemented!")).unwrap();

		let x = piece.pos % 8;
		let y = 7 - piece.pos / 8;
		let mat_model = Matrix4::new_scaling(scale) * Matrix4::new_translation(&Vector3::<f32>::new(x as f32 * 0.25, y as f32 * 0.25, 0.0));

		self.texture_program.set_uniform("u_mvp", &(view_proj * mat_model)).unwrap();
		render_elements(&self.texture_program, &self.texture_mesh.vao, &self.texture_mesh.ibo);
	}
}
