use crate::board::*;

/* Chess AI */
fn sort_moves(board: &Board, moves: &mut Vec<(&Piece, Pos)>) {
	moves.sort_by(|(piece_a, piece_move_a), (piece_b, piece_move_b)| {
		let val_a = evaluate_move_static(board, piece_a, *piece_move_a);
		let val_b = evaluate_move_static(board, piece_b, *piece_move_b);

		return val_a
			.partial_cmp(&val_b)
			.unwrap_or(std::cmp::Ordering::Equal);
	});
}

/// Deprecated, same as alpha_beta but slower
pub fn minmax(board: Board, depth: usize) -> f32 {
	if depth == 0 {
		return evaluate_static(&board, board.active_color);
	}

	let mut possible_moves = Vec::new();

	for piece in board.pieces().iter() {
		for piece_move in board.generate_legal_moves(&piece).unwrap() {
			possible_moves.push((piece, piece_move));
		}
	}

	if possible_moves.len() == 0 {
		if board.is_check(board.active_color) {
			println!("DANGER/CHECKMATE");
			return f32::NEG_INFINITY;
		}

		return 0.0;
	}

	let mut best_eval = f32::NEG_INFINITY;

	for (piece, piece_move) in possible_moves {
		let mut board_clone = board.clone();
		if board_clone.make_move(piece, piece_move).ok().is_some() {
			board_clone.active_color = board.active_color.opposite();

			let eval = -minmax(board_clone, depth - 1);
			best_eval = best_eval.max(eval);
		}
	}

	return best_eval;
}

pub fn alpha_beta(board: Board, depth: usize, mut alpha: f32, beta: f32, sorted: bool) -> f32 {
	if depth == 0 {
		return evaluate_static(&board, board.active_color);
	}

	let mut possible_moves = Vec::new();

	for piece in board.pieces().iter() {
		for piece_move in board.generate_legal_moves(&piece).unwrap() {
			possible_moves.push((piece, piece_move));
		}
	}

	if sorted {
		sort_moves(&board, &mut possible_moves);
	}

	if possible_moves.len() == 0 {
		if board.is_check(board.active_color) {
			println!("DANGER/CHECKMATE");
			return f32::NEG_INFINITY;
		}

		return 0.0;
	}

	//let mut best_eval = f32::NEG_INFINITY;

	for (piece, piece_move) in possible_moves {
		let mut board_clone = board.clone();
		if board_clone.make_move(piece, piece_move).ok().is_some() {
			board_clone.active_color = board.active_color.opposite();

			let eval = -alpha_beta(board_clone, depth - 1, -beta, -alpha, sorted);

			alpha = alpha.max(eval);
			if beta <= alpha {
				break;
			}
			//best_eval = best_eval.max(eval);
		}
	}

	return alpha;
}

fn piece_value(piece: &Piece) -> f32 {
	return match piece.piece_type() {
		PieceType::Pawn => 1.0,
		PieceType::Knight => 3.0,
		PieceType::Bishop => 3.0,
		PieceType::Rook => 5.0,
		PieceType::Queen => 7.0,
		PieceType::King => 0.0,
	};
}

fn evaluate_static(board: &Board, color: Color) -> f32 {
	let mut result = 0.0;

	for piece in board.pieces().iter() {
		let value = piece_value(piece);

		if piece.color() == color {
			result += value;
		} else {
			result -= value;
		}
	}

	return result;
}

fn evaluate_move_static(board: &Board, piece: &Piece, pos: Pos) -> f32 {
	if let Some(capture) = board.piece_at(pos) {
		return piece_value(&capture) - piece_value(piece);
	}

	if piece.piece_type() == PieceType::Pawn {
		let center_dist = (piece.pos().x() as f32 - 4.0).abs();

		return (4.0 - center_dist) / 4.0;
	}

	return 0.0;
}
