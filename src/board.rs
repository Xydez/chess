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

impl std::fmt::Display for PieceType
{
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		return match self
		{
			PieceType::Pawn => write!(f, "Pawn"),
			PieceType::Rook => write!(f, "Rook"),
			PieceType::Knight => write!(f, "Knight"),
			PieceType::Bishop => write!(f, "Bishop"),
			PieceType::Queen => write!(f, "Queen"),
			PieceType::King => write!(f, "King")
		};
	}
}

impl PieceType
{

}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Color
{
	White, Black
}

impl std::fmt::Display for Color
{
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		return match self
		{
			Color::White => write!(f, "White"),
			Color::Black => write!(f, "Black"),
		};
	}
}

impl Color
{
	pub fn opposite(&self) -> Color
	{
		return match self
		{
			Color::Black => Color::White,
			Color::White => Color::Black
		};
	}
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
		write!(f, "{}", self.piece_type)?;

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

	pub fn piece_type(&self) -> PieceType
	{
		return self.piece_type;
	}
}

pub fn move_notation(piece: &Piece, target: Pos) -> String
{
	if piece.piece_type() == PieceType::Pawn
	{
		return target.to_algebraic();
	}
	else
	{
		return format!("{}{}", piece.to_char(), target);
	}
}

#[derive(Debug, Clone)]
pub struct Board
{
	pub active_color: Color,
	pieces: Vec<Piece>,
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

	pub fn piece_at_mut(&mut self, pos: Pos) -> Option<&mut Piece>
	{
		for piece in self.pieces.iter_mut()
		{
			if piece.pos == pos
			{
				return Some(piece);
			}
		}
		
		return None;
	}

	pub fn find_pieces(&self, piece_type: Option<PieceType>, color: Option<Color>, position: Option<Pos>) -> Vec<&Piece>
	{
		return self.pieces.iter().filter(|piece|
		{
			if let Some(piece_type) = piece_type
			{
				if piece.piece_type != piece_type
				{
					return false;
				}
			}

			if let Some(color) = color
			{
				if piece.color != color
				{
					return false;
				}
			}

			if let Some(position) = position
			{
				if piece.pos != position
				{
					return false;
				}
			}

			return true;
		}).collect();
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
		// If we're moving onto a piece, capture that piece
		let capture = if let Some(target_piece) = self.piece_at(target)
		{
			if target_piece.color() == piece.color()
			{
				// We can't capture pieces of the same color
				return Err(());
			}
			
			self.remove_at(target_piece.pos()).unwrap();
			true
		}
		else
		{
			false
		};

		// Find the piece in self.pieces and mutate that
		for piece_ref in self.pieces.iter_mut()
		{
			if *piece_ref == *piece
			{
				piece_ref.pos = target;

				// Check if castled
				if piece.piece_type == PieceType::King && (target.x() as i32 - piece.pos.x() as i32) == 2
				{
					// Castled kingside (2 squares to the right)
					if let Some(rook) = self.piece_at_mut(Pos::new(7, piece.pos.y()))
					{
						assert!(rook.piece_type == PieceType::Rook);

						rook.pos = Pos::new(5, piece.pos.y());
					}
					else
					{
						unimplemented!();
					}
				}
				else if piece.piece_type == PieceType::King && (target.x() as i32 - piece.pos.x() as i32) == -2
				{
					// Castled queenside (2 squares to the left)

					if let Some(rook) = self.piece_at_mut(Pos::new(0, piece.pos.y()))
					{
						assert!(rook.piece_type == PieceType::Rook);

						rook.pos = Pos::new(3, piece.pos.y());
					}
				}

				if piece.piece_type == PieceType::Pawn
				{
					let move_diff_y = target.y() as i32 - piece.pos.y() as i32;

					// Check if double move, if so create an en passant target
					if move_diff_y.abs() == 2
					{
						self.en_passant_target = Some(Pos::new(piece.pos.x(), (piece.pos.y() as i32 + move_diff_y / 2) as u8));
						println!("En passant target is now at {}", self.en_passant_target.unwrap());
						return Ok(());
					}
					else if let Some(en_passant_target) = self.en_passant_target
					{
						// If en passant was peformed, capture the piece
						if target == en_passant_target
						{
							match piece.color()
							{
								Color::Black =>
								{
									self.remove_at(Pos::new(en_passant_target.x(), en_passant_target.y() - 1)).expect("Invalid en passant");
								},
								Color::White =>
								{
									self.remove_at(Pos::new(en_passant_target.x(), en_passant_target.y() + 1)).expect("Invalid en passant");
								}
							}
						}
					}
				}

				self.en_passant_target = None;
				return Ok(());
			}
		}

		return Err(());
	}

	pub fn generate_legal_moves(&self, piece: &Piece) -> Result<Vec<Pos>, ()>
	{
		if let Some(piece) = self.piece_at(piece.pos)
		{
			let moves = match piece.piece_type
			{
				PieceType::Rook => self.generate_straight_moves(piece.pos),
				PieceType::Bishop => self.generate_diagonal_moves(piece.pos),
				PieceType::Queen => self.generate_straight_moves(piece.pos).into_iter().chain(self.generate_diagonal_moves(piece.pos).into_iter()).collect(),
				/*
				PieceType::Queen => {
					let mut moves = self.generate_straight_moves(piece.pos);
					moves.append(&mut self.generate_diagonal_moves(piece.pos));

					moves
				},
				*/
				PieceType::King => self.generate_king_moves(piece.color, piece.pos),
				PieceType::Knight => self.generate_knight_moves(piece.pos),
				PieceType::Pawn => self.generate_pawn_moves(piece.color, piece.pos)
			};
			
			// Filter the moves 
			let moves = moves.into_iter().filter(|pos|
			{
				if let Some(pos_piece) = self.piece_at(*pos)
				{
					if pos_piece.color() == piece.color()
					{
						return false;
					}
				}
	
				return true;
			}).collect();

			// TODO:
			// After filtering the moves, add optional castling
			// Another way is to separate castling from the legal moves

			return Ok(moves);
		}
		else
		{
			return Err(());
		}
	}

	fn generate_straight_moves(&self, pos: Pos) -> Vec<Pos>
	{
		let mut moves = Vec::new();

		// Left
		for x in (0..pos.x()).rev()
		{
			let move_pos = Pos::new(x, pos.y());

			moves.push(move_pos);

			if self.piece_at(move_pos).is_some()
			{
				break;
			}
		}

		// Right
		for x in (pos.x() + 1)..8
		{
			let move_pos = Pos::new(x, pos.y());

			moves.push(move_pos);

			if self.piece_at(move_pos).is_some()
			{
				break;
			}
		}

		// Up
		for y in (0..pos.y()).rev()
		{
			let move_pos = Pos::new(pos.x(), y);

			moves.push(move_pos);

			if self.piece_at(move_pos).is_some()
			{
				break;
			}
		}

		// Down
		for y in (pos.y() + 1)..8
		{
			let move_pos = Pos::new(pos.x(), y);

			moves.push(move_pos);

			if self.piece_at(move_pos).is_some()
			{
				break;
			}
		}

		return moves;
	}

	fn generate_diagonal_moves(&self, pos: Pos) -> Vec<Pos>
	{
		let mut moves = Vec::new();
		let mut i;

		// Top left
		i = 1;
		loop
		{
			let x = pos.x() as i32 - i;
			let y = pos.y() as i32 - i;

			if x < 0 || y < 0
			{
				break;
			}

			moves.push(Pos::new(x as u8, y as u8));

			i += 1;
		}

		// Top right
		i = 1;
		loop
		{
			let x = pos.x() as i32 + i;
			let y = pos.y() as i32 - i;

			if x > 7 || y < 0
			{
				break;
			}

			moves.push(Pos::new(x as u8, y as u8));

			i += 1;
		}

		// Bottom right
		i = 1;
		loop
		{
			let x = pos.x() as i32 + i;
			let y = pos.y() as i32 + i;

			if x > 7 || y > 7
			{
				break;
			}

			moves.push(Pos::new(x as u8, y as u8));

			i += 1;
		}

		// Bottom left
		i = 1;
		loop
		{
			let x = pos.x() as i32 - i;
			let y = pos.y() as i32 + i;

			if x < 0 || y > 7
			{
				break;
			}

			moves.push(Pos::new(x as u8, y as u8));

			i += 1;
		}

		return moves;
	}

	fn generate_king_moves(&self, color: Color, pos: Pos) -> Vec<Pos>
	{
		let mut moves = Vec::new();

		for xo in -1..=1
		{
			for yo in -1..=1
			{
				let x = pos.x() as i32 + xo;
				let y = pos.y() as i32 + yo;

				if x >= 0 && x < 8 && y >= 0 && y < 8
				{
					moves.push(Pos::new(x as u8, y as u8));
				}
			}
		}

		//println!("{} {} {} {}", self.black_castle_kingside, self.black_castle_queenside, self.white_castle_kingside, self.white_castle_queenside);

		// TODO
		if color == Color::Black
		{
			if self.black_castle_kingside
			{
				if self.piece_at(Pos::new(5, 0)).is_none() && self.piece_at(Pos::new(6, 0)).is_none()
				{
					moves.push(Pos::new(6, 0));
				}
			}

			if self.black_castle_queenside
			{
				if self.piece_at(Pos::new(1, 0)).is_none() && self.piece_at(Pos::new(2, 0)).is_none() && self.piece_at(Pos::new(3, 0)).is_none()
				{
					moves.push(Pos::new(2, 0));
				}
			}
		}
		else
		{
			if self.white_castle_kingside
			{
				if self.piece_at(Pos::new(5, 7)).is_none() && self.piece_at(Pos::new(6, 7)).is_none()
				{
					moves.push(Pos::new(6, 7));
				}
			}

			if self.white_castle_queenside
			{
				if self.piece_at(Pos::new(1, 7)).is_none() && self.piece_at(Pos::new(2, 7)).is_none() && self.piece_at(Pos::new(3, 7)).is_none()
				{
					moves.push(Pos::new(2, 7));
				}
			}
		}

		return moves;
	}

	fn generate_knight_moves(&self, pos: Pos) -> Vec<Pos>
	{
		let mut moves = Vec::new();

		for xo in -2..=2
		{
			for yo in -2..=2
			{
				if (xo as i32).abs().min((yo as i32).abs()) == 1 && (xo as i32).abs().max((yo as i32).abs()) == 2
				{
					let x = pos.x() as i32 + xo;
					let y = pos.y() as i32 + yo;

					if x >= 0 && x < 8 && y >= 0 && y < 8
					{
						moves.push(Pos::new(x as u8, y as u8));
					}
				}
			}
		}

		return moves;
	}

	fn generate_pawn_moves(&self, color: Color, pos: Pos) -> Vec<Pos>
	{
		let mut moves = Vec::new();

		match color
		{
			// We don't need to check if the pawn has reached the end because it will be automatically transformed to a queen
			Color::Black =>
			{
				// Normal move
				moves.push(Pos::new(pos.x(), pos.y() + 1));

				// Capture
				if pos.x() > 0
				{
					if let Some(piece) = self.piece_at(Pos::new(pos.x() - 1, pos.y() + 1))
					{
						if piece.color != color
						{
							moves.push(piece.pos());
						}
					}
				}

				if pos.x() < 7
				{
					if let Some(piece) = self.piece_at(Pos::new(pos.x() + 1, pos.y() + 1))
					{
						if piece.color != color
						{
							moves.push(piece.pos());
						}
					}
				}

				// En passant
				if let Some(target) = self.en_passant_target
				{
					if target.y() == pos.y() + 1
					{
						if target.x() == pos.x() + 1 || target.x() + 1 == pos.x()
						{
							moves.push(target);
						}
					}
				}

				// Double move
				if pos.y() == 1
				{
					moves.push(Pos::new(pos.x(), pos.y() + 2));
				}
			},
			Color::White =>
			{
				// Normal move
				moves.push(Pos::new(pos.x(), pos.y() - 1));

				// Capture
				if pos.x() > 0
				{
					if let Some(piece) = self.piece_at(Pos::new(pos.x() - 1, pos.y() - 1))
					{
						if piece.color != color
						{
							moves.push(piece.pos());
						}
					}
				}

				if pos.x() < 7
				{
					if let Some(piece) = self.piece_at(Pos::new(pos.x() + 1, pos.y() - 1))
					{
						if piece.color != color
						{
							moves.push(piece.pos());
						}
					}
				}

				// En passant
				if let Some(target) = self.en_passant_target
				{
					if target.y() == pos.y() - 1
					{
						if target.x() == pos.x() + 1 || target.x() + 1 == pos.x()
						{
							moves.push(target);
						}
					}
				}

				// Double move
				if pos.y() == 6
				{
					moves.push(Pos::new(pos.x(), pos.y() - 2));
				}
			}
		}

		return moves;
	}

	pub fn is_check(&self, color: Color) -> bool
	{
		// TODO
		let king_pos = self.pieces.iter().find(|piece| piece.piece_type == PieceType::King && piece.color == color).and_then(|piece| Some(piece.pos())).expect(format!("Could not find {}'s king", color).as_str());

		// Iterate over all of the enemy's pieces
		for piece in self.pieces.iter().filter(|piece| piece.color() != color)
		{
			for target in self.generate_legal_moves(&piece).unwrap()
			{
				if target == king_pos
				{
					return true;
				}
			}
		}

		return false;
	}

	pub fn is_legal_move(&self, piece: &Piece, pos: Pos) -> bool
	{
		for piece_move in self.generate_legal_moves(&piece).unwrap()
		{
			if piece_move == pos
			{
				let mut board = self.clone();
				board.make_move(piece, pos).unwrap();
				
				return !board.is_check(piece.color());
			}
		}

		return false;
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

	pub fn render(&mut self, board: &Board, scale: f32)
	{
		let view_proj = self.calculate_vp_matrix(scale);

		self.render_board(&view_proj, scale);

		for piece in board.pieces.iter()
		{
			self.render_piece(&view_proj, scale, piece);
		}
	}

	fn calculate_vp_matrix(&self, scale: f32) -> Matrix4<f32>
	{
		let mat_view = Matrix4::new_translation(&Vector3::<f32>::new(0.0, 0.0, -1.0));

		let w = self.aspect.max(1.0);
		let h = 1.0f32.max(1.0 / self.aspect);
		let mat_proj = Matrix4::new_orthographic(-w, w, -h, h, 0.1, 100.0);

		let view_proj = mat_proj * mat_view;

		return view_proj;
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
	
	pub fn render_square(&mut self, scale: f32, position: Pos, color: Vector4<f32>)
	{
		let view_proj = self.calculate_vp_matrix(scale);

		self.render_square_internal(&view_proj, scale, position, color);
	}

	fn render_square_internal(&mut self, view_proj: &Matrix4<f32>, scale: f32, pos: Pos, color: Vector4<f32>)
	{
		self.square_program.set_uniform("color", &color).unwrap();

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
