use std::error::Error;

use chess::{Board,Color::*,Loc,Piece,PieceKind::*};

fn main() -> Result<(), Box<dyn Error>> {
    let mut b = Board::new();
    println!("{}", b);
    b.set_piece_str("white", "pawn", "A1")?;
    println!("{}", b);
    b.set_piece_str("black", "king", "H8")?;
    println!("{}", b);
    println!("{:?}", b);
    let mut b = Board::new();
    b.setup()?;
    println!("{}", b);
    b.move_piece_str("E2", "E4")?;
    println!("{}", b);
    b.move_piece_str("D7", "D5")?;
    println!("{}", b);
    println!("{:?}", b.get_targets("E4"));
    b.move_piece_str("E4", "D5")?;
    println!("{}", b);
    b.move_piece_str("A1", "A2")?;
    println!("{}", b);
    Ok(())
}
