use crate::renderer::*;
use crate::gl;

use nalgebra::{ Matrix4, Vector4, Vector3 };
use std::path::Path;
use std::collections::HashMap;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Pos
{
	pub value: u8
}

impl std::fmt::Display for Pos
{
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(f, "{}", self.to_algebraic())?;

		return Ok(());
	}
}

impl Pos
{
	pub fn new(x: u8, y: u8) -> Pos
	{
		assert!(x < 8 && y < 8);

		return Pos { value: y * 8 + x };
	}

	pub fn from_index(i: u8) -> Pos
	{
		return Pos { value: i };
	}

	pub fn from_algebraic(value: &str) -> Result<Pos, ()>
	{
		if value.len() != 2
		{
			return Err(());
		}

		let letter = value.chars().nth(0).unwrap();
		if letter < 'a' || letter > 'h'
		{
			return Err(());
		}

		let number = value.chars().nth(1).unwrap();
		if number < '1' || number > '8'
		{
			return Err(());
		}

		return Ok(Pos::new(letter as u8 - 'a' as u8, number as u8 - '1' as u8));
	}

	pub fn to_algebraic(&self) -> String
	{
		return format!("{}{}", ('a' as u8 + self.x()) as char, ('1' as u8 + 7 - self.y()) as char);
	}

	pub fn x(&self) -> u8
	{
		return self.value % 8;
	}

	pub fn y(&self) -> u8
	{
		return self.value / 8;
	}
}

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

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Piece
{
	piece_type: PieceType,
	color: Color,
	pos: Pos
}

impl std::fmt::Display for Piece
{
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(f, "{}", self.to_char())?;

		return Ok(());
	}
}

impl Piece
{
	pub fn new(color: Color, piece_type: PieceType, pos: Pos) -> Piece
	{
		return Piece
		{
			piece_type, color, pos
		};
	}

	pub fn from_char(c: char, pos: Pos) -> Result<Piece, ()>
	{
		let color = if c.is_ascii_lowercase() { Color::Black } else { Color::White };

		let piece_type = match c.to_ascii_lowercase()
		{
			'b' => Ok(PieceType::Bishop),
			'k' => Ok(PieceType::King),
			'n' => Ok(PieceType::Knight),
			'p' => Ok(PieceType::Pawn),
			'q' => Ok(PieceType::Queen),
			'r' => Ok(PieceType::Rook),
			_ => Err(())
		};

		if piece_type.is_err()
		{
			return Err(());
		}

		return Ok(Piece::new(color, piece_type.unwrap(), pos));
	}

	pub fn to_char(&self) -> char
	{
		let letter = match self.piece_type
		{
			PieceType::Bishop => 'b',
			PieceType::King => 'k',
			PieceType::Knight => 'n',
			PieceType::Pawn => 'p',
			PieceType::Queen => 'q',
			PieceType::Rook => 'r',
		};

		return match self.color
		{
			Color::White => letter.to_ascii_uppercase(),
			Color::Black => letter.to_ascii_lowercase()
		};
	}

	pub fn pos(&self) -> Pos
	{
		return self.pos;
	}

	pub fn color(&self) -> Color
	{
		return self.color;
	}
}

#[derive(Debug)]
pub struct Board
{
	pieces: Vec<Piece>,
	active_color: Color,
	black_castle_kingside: bool,
	black_castle_queenside: bool,
	white_castle_kingside: bool,
	white_castle_queenside: bool,
	en_passant_target: Option<Pos>,
	halfmove_clock: usize,
	fullmove_clock: usize
}

impl Board
{
	pub fn empty() -> Board
	{
		return Board
		{
			pieces: Vec::new(),
			active_color: Color::White,
			black_castle_kingside: false,
			black_castle_queenside: false,
			white_castle_kingside: false,
			white_castle_queenside: false,
			en_passant_target: None,
			halfmove_clock: 0,
			fullmove_clock: 0
		};
	}

	pub fn from_fen(fen: &str) -> Result<Board, ()>
	{
		let mut board = Board::empty();

		let parts: Vec<&str> = fen.split(" ").collect();
		if parts.len() != 6
		{
			return Err(());
		}

		// 1. Piece placement
		let ranks: Vec<&str> = parts[0].split("/").collect();
		if ranks.len() != 8
		{
			return Err(());
		}

		for (y, rank) in ranks.iter().enumerate()
		{
			let mut x = 0;

			for c in rank.chars()
			{
				if c.is_numeric()
				{
					x += c.to_string().parse::<u8>().unwrap();
				}
				else
				{
					board.add(Piece::from_char(c, Pos::new(x as u8, y as u8))?).unwrap();

					x += 1;
				}
			}

			if x != 8
			{
				return Err(());
			}
		}

		// 2. Active color
		let color = match parts[1]
		{
			"w" => Ok(Color::White),
			"b" => Ok(Color::Black),
			_ => Err(())
		};

		if color.is_err()
		{
			return Err(());
		}

		board.active_color = color.unwrap();

		// 3. Castling availability
		if parts[2] != "-"
		{
			for c in parts[2].chars()
			{
				match c
				{
					'K' => { board.white_castle_kingside = true; },
					'Q' => { board.white_castle_queenside = true; },
					'k' => { board.black_castle_kingside = true; },
					'q' => { board.black_castle_queenside = true; },
					_ => { return Err(()); }
				}
			}
		}

		// 4. En passant target square
		if parts[3] == "-"
		{
			board.en_passant_target = None;
		}
		else
		{
			let target = Pos::from_algebraic(parts[3])?;

			board.en_passant_target = Some(target);
		}

		// 5. Halfmove clock
		let halfmove = parts[4].parse::<usize>();
		if halfmove.is_err()
		{
			return Err(());
		}

		board.halfmove_clock = halfmove.unwrap();

		// 6. Fullmove number
		let fullmove = parts[5].parse::<usize>();
		if fullmove.is_err()
		{
			return Err(());
		}

		board.fullmove_clock = fullmove.unwrap();

		return Ok(board);
	}

	pub fn standard() -> Board
	{
		return Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap();
	}

	pub fn piece_at(&self, pos: Pos) -> Option<Piece>
	{
		for piece in self.pieces.iter()
		{
			if piece.pos == pos
			{
				return Some(*piece);
			}
		}

		return None;
	}

	pub fn add(&mut self, piece: Piece) -> Result<(), ()>
	{
		// Check if there is already a piece at the position
		if self.piece_at(piece.pos).is_some()
		{
			return Err(());
		}
		else
		{
			self.pieces.push(piece);
			
			return Ok(());
		}
	}

	pub fn remove_at(&mut self, pos: Pos) -> Result<Piece, ()>
	{
		if let Some(i) = self.pieces.iter().position(|x| x.pos() == pos)
		{
			return Ok(self.pieces.remove(i));
		}
		else
		{
			return Err(());
		}
	}

	pub fn make_move(&mut self, piece: &Piece, target: Pos) -> Result<(), ()>
	{
		if self.piece_at(target).is_some()
		{
			return Err(());
		}

		for piece_ref in self.pieces.iter_mut()
		{
			if *piece_ref == *piece
			{
				piece_ref.pos = target;

				return Ok(());
			}
		}

		return Err(());
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

		texture_map.insert((Color::White, PieceType::Bishop), Texture::load(Path::new("textures/lb.png")).unwrap());
		texture_map.insert((Color::White, PieceType::King), Texture::load(Path::new("textures/lk.png")).unwrap());
		texture_map.insert((Color::White, PieceType::Knight), Texture::load(Path::new("textures/ln.png")).unwrap());
		texture_map.insert((Color::White, PieceType::Pawn), Texture::load(Path::new("textures/lp.png")).unwrap());
		texture_map.insert((Color::White, PieceType::Queen), Texture::load(Path::new("textures/lq.png")).unwrap());
		texture_map.insert((Color::White, PieceType::Rook), Texture::load(Path::new("textures/lr.png")).unwrap());

		texture_map.insert((Color::Black, PieceType::Bishop), Texture::load(Path::new("textures/db.png")).unwrap());
		texture_map.insert((Color::Black, PieceType::King), Texture::load(Path::new("textures/dk.png")).unwrap());
		texture_map.insert((Color::Black, PieceType::Knight), Texture::load(Path::new("textures/dn.png")).unwrap());
		texture_map.insert((Color::Black, PieceType::Pawn), Texture::load(Path::new("textures/dp.png")).unwrap());
		texture_map.insert((Color::Black, PieceType::Queen), Texture::load(Path::new("textures/dq.png")).unwrap());
		texture_map.insert((Color::Black, PieceType::Rook), Texture::load(Path::new("textures/dr.png")).unwrap());

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

	pub fn render(&mut self, board: &Board, scale: f32, cursor: Option<Pos>)
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

	fn render_cursor(&mut self, view_proj: &Matrix4<f32>, scale: f32, pos: Pos)
	{
		self.square_program.set_uniform("color", &Vector3::new(0.68, 0.92, 0.63)).unwrap();

		let x = pos.x();
		let y = 7 - pos.y();
		let mat_model = Matrix4::new_scaling(scale) * Matrix4::new_translation(&Vector3::<f32>::new(x as f32 * 0.25, y as f32 * 0.25, 0.0));

		self.square_program.set_uniform("mvp", &(view_proj * mat_model)).unwrap();
		render_elements(&self.square_program, &self.mesh.vao, &self.mesh.ibo);
	}

	fn render_piece(&mut self, view_proj: &Matrix4<f32>, scale: f32, piece: &Piece)
	{
		self.texture_program.set_uniform("u_texture_in", self.piece_textures.get(piece).expect("Piece texture not implemented!")).unwrap();

		let x = piece.pos.x();
		let y = 7 - piece.pos.y();
		let mat_model = Matrix4::new_scaling(scale) * Matrix4::new_translation(&Vector3::<f32>::new(x as f32 * 0.25, y as f32 * 0.25, 0.0));

		self.texture_program.set_uniform("u_mvp", &(view_proj * mat_model)).unwrap();
		render_elements(&self.texture_program, &self.texture_mesh.vao, &self.texture_mesh.ibo);
	}
}
