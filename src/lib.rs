use std::error;
use std::fmt;
use std::iter;
use std::str::FromStr;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Loc {
    x: usize,
    y: usize,
}

impl Loc {
    pub fn new(x: i8, y: i8) -> Result<Loc, Box<dyn error::Error>> {
        if x < 0 || x > 7 {
            Err(format!("Invalid row: {}", x).into())
        } else if y < 0 || y > 7 {
            Err(format!("Invalid column: {}", y).into())
        } else {
            Ok( Loc { x: x as usize, y: y as usize } )
        }
    }
}

impl FromStr for Loc {
    type Err = Box<dyn error::Error>;
    fn from_str(s: &str) -> Result<Loc, Self::Err> {
        let s = s.to_lowercase();
        if s.len() != 2 {
            return Err(format!("Invalid loc string: {}", s).into())
        }
        let mut chars = s.chars();
        let row = chars.next().unwrap();
        if row < 'a' || row > 'h' {
            return Err(format!("Invalid row: {}", row).into());
        }
        let row = row as i8 - 'a' as i8;
        let column = chars.next().unwrap();
        if column < '1' || column > '8' {
            return Err(format!("Invalid column: {}", column).into());
        }
        let column = column as i8 - '1' as i8;
        Loc::new(row, column)
    }
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let row = "ABCDEFGH".chars().nth(self.x).unwrap();
        let column = 1 + self.y;
        write!(f, "{}{}", row, column)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Color {
    White,
    Black,
}

impl FromStr for Color {
    type Err = Box<dyn error::Error>;
    fn from_str(s: &str) -> Result<Color, Self::Err> {
        match s.to_lowercase().as_str() {
            "white" => Ok(White),
            "black" => Ok(Black),
            _ => Err(format!("Invalid color string: {}", s).into()),
        }
    }
}

use Color::*;

#[derive(Debug)]
pub enum PieceKind {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

impl FromStr for PieceKind {
    type Err = Box<dyn error::Error>;
    fn from_str(s: &str) -> Result<PieceKind, Self::Err> {
        match s.to_lowercase().as_str() {
            "pawn" => Ok(Pawn),
            "knight" => Ok(Knight),
            "bishop" => Ok(Bishop),
            "rook" => Ok(Rook),
            "queen" => Ok(Queen),
            "king" => Ok(King),
            _ => Err(format!("Invalid piece kind string: {}", s).into()),
        }
    }
}

use PieceKind::*;

#[derive(Debug)]
pub struct Piece {
    has_moved: bool,
    color: Color,
    kind: PieceKind,
}

impl Piece {
    pub fn new(color: Color, kind: PieceKind) -> Piece {
        Piece {
            has_moved: false,
            color: color,
            kind: kind,
        }
    }

    pub fn symbol(&self) -> String {
        match (&self.kind, &self.color) {
            (Pawn, White) => "♙",
            (Pawn, Black) => "♟",
            (Knight, White) => "♘",
            (Knight, Black) => "♞",
            (Bishop, White) => "♗",
            (Bishop, Black) => "♝",
            (Rook, White) => "♖",
            (Rook, Black) => "♜",
            (Queen, White) => "♕",
            (Queen, Black) => "♛",
            (King, White) => "♔",
            (King, Black) => "♚",
        }.to_string()
    }
}

#[derive(Debug)]
pub struct Board {
    locs: [[Option<Piece>; 8]; 8],
}

impl Board {
	pub fn new() -> Board {
		Board { locs: Default::default() }
    }

    fn loc(&self, loc: Loc) -> Option<&Piece> {
        self.locs[loc.x][loc.y].as_ref()
    }

    fn empty(&self, loc: Loc) -> bool {
        self.loc(loc).is_none()
    }

    fn occupant(&self, loc: Loc) -> Result<&Piece, Box<dyn error::Error>> {
        match self.loc(loc) {
            Some(piece) => Ok(piece),
            None => Err(format!("No piece found at {}", loc).into()),
        }
    }

    fn take_piece(&mut self, loc: Loc)
        -> Result<Piece, Box<dyn error::Error>> {
        match self.locs[loc.x][loc.y].take() {
            Some(piece) => Ok(piece),
            None => Err(format!("No piece at location: {}", loc).into()),
        }
    }

    fn set_piece(&mut self, piece: Piece, loc: Loc) {
        self.locs[loc.x][loc.y] = Some(piece);
    }

    fn move_piece(&mut self, from: Loc, to: Loc)
        -> Result<(), Box<dyn error::Error>> {
        let mut piece = self.take_piece(from)?;
        piece.has_moved = true;
        self.set_piece(piece, to);
        Ok(())
    }

    pub fn set_piece_str(&mut self, color: &str, kind: &str, loc: &str)
        -> Result<(), Box<dyn error::Error>> {
        let piece = Piece::new(color.parse()?, kind.parse()?);
        let loc = loc.parse()?;
        self.set_piece(piece, loc);
        Ok(())
    }

    pub fn move_piece_str(&mut self, from: &str, to: &str)
        -> Result<(), Box<dyn error::Error>> {
        self.move_piece(from.parse()?, to.parse()?)
    }

    fn apply_move(&self, cur_loc: Loc, x: i8, y: i8)
        -> Result<Loc, Box<dyn error::Error>> {
        let x = cur_loc.x as i8 + x;
        let y = cur_loc.y as i8 + y;
        Loc::new(x, y)
    }

    fn pawn_moves(&self, pawn: &Piece, cur_loc: Loc) -> Vec<Loc> {
        let mut targets = vec![];

        let direction = match pawn.color {
            Color::White => 1,
            Color::Black => -1,
        };

        // First move, forward two spaces
        if !pawn.has_moved {
            if let Ok(target) = self.apply_move(cur_loc, 0, 2 * direction) {
                if self.empty(target) {
                    targets.push(target);
                }
            }
        }

        // Subsequent move, forward one space
        if let Ok(target) = self.apply_move(cur_loc, 0, 1 * direction) {
            if self.empty(target) {
                targets.push(target);
            }
        }

        // Attack move, diagonal forward one space
        for target in [
            self.apply_move(cur_loc, -1, 1 * direction),
            self.apply_move(cur_loc, 1, 1 * direction)
        ] {
            if let Ok(t) = target {
                if let Ok(piece) = self.occupant(t) {
                    if piece.color != pawn.color {
                        targets.push(t);
                    }
                }
            }
        }

        // To-do: En passant
        // (need to track when a piece moves to en passant position)
 
        targets 
    }

    fn targets(&self, piece: &Piece, cur_loc: Loc) -> Vec<Loc> {
        match piece.kind {
            Pawn => self.pawn_moves(piece, cur_loc),
            // Knight => self.knight_moves(piece),
            // Bishop => self.bishop_moves(piece),
            // Rook => self.rook_moves(piece),
            // Queen => self.queen_moves(piece),
            // King => self.king_moves(piece),
            _ => vec![], // Delete this when all the above are filled
        }
    }

    pub fn get_targets(&self, loc: &str)
        -> Result<Vec<Loc>, Box<dyn error::Error>> {
        let loc = loc.parse()?;
        let piece = self.occupant(loc)?;
        let targets = self.targets(piece, loc);
        Ok(targets)
    }

    pub fn setup(&mut self) -> Result<(), Box<dyn error::Error>> {
        let mut values = vec![];
        for (color, row) in [(White, 0), (Black, 7)] {
            for v in [
                (color, Rook, 0, row),
                (color, Knight, 1, row),
                (color, Bishop, 2, row),
                (color, Queen, 3, row),
                (color, King, 4, row),
                (color, Bishop, 5, row),
                (color, Knight, 6, row),
                (color, Rook, 7, row) ] {
                values.push(v);
            }
        }

        for (color, row) in [(White, 1), (Black, 6)] {
            for column in 0..8 {
                values.push((color, Pawn, column, row));
            }
        }

        for (color, kind, column, row) in values {
            let piece = Piece::new(color, kind);
            let loc = Loc::new(column, row)?;
            self.set_piece(piece, loc);
        }

        Ok(())
    }
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let line: String = iter::repeat('-').take(33).collect();
        let mut buffer = format!("  {}\n", line);
        for y in (0..8).rev() {
            buffer = format!("{}{} ", buffer, y + 1);
            for x in 0..8 {
                let symbol = match &self.locs[x][y] {
                    Some(piece) => piece.symbol(),
                    None => " ".to_string(),
                };
                buffer = format!("{}| {} ", buffer, symbol);
            }
            buffer = format!("{}|\n  {}\n", buffer, line);
        }
        buffer = format!("{}    A   B   C   D   E   F   G   H\n", buffer);
        write!(f, "{}", buffer)
    }
}

#[cfg(test)]
mod tests {
	use super::*;

    #[test]
    fn loc_from_str() -> Result<(), Box<dyn error::Error>> {
        fn expected_error(s: &str, msg: &str) -> Result<(), Box<dyn error::Error>> {
            match s.parse::<Loc>() {
                Err(e) => {
                    if format!("{}", e).contains(msg) {
                        Ok(())
                    } else {
                        Err(format!("Wrong error message for {}: {}", s, e).into())
                    }
                }
                _ => Err(format!("{} didn't raise an error", s).into()),
            }
        }

        assert_eq!(Loc::new(0,0)?, "A1".parse()?);
        assert_eq!(Loc::new(7,7)?, "H8".parse()?);
        assert_eq!(Loc::new(0,7)?, "A8".parse()?);
        assert_eq!(Loc::new(7,0)?, "H1".parse()?);

        assert_eq!(Loc::new(0,0)?, "a1".parse()?);
        assert_eq!(Loc::new(7,7)?, "h8".parse()?);
        assert_eq!(Loc::new(0,7)?, "a8".parse()?);
        assert_eq!(Loc::new(7,0)?, "h1".parse()?);

        expected_error("I1", "Invalid row: i")?;
        expected_error("X1", "Invalid row: x")?;
        expected_error("A0", "Invalid column: 0")?;
        expected_error("A9", "Invalid column: 9")?;
        expected_error("AA", "Invalid column: a")?;
        Ok(())
    }
}
