use lexer::Lexer;
use std::{
    fs::read_to_string,
    io::{Error, ErrorKind},
};

mod lexer;
mod string_table;
mod ast;
fn main() -> Result<(), Error> {
    let path = match std::env::args().nth(1) {
        Some(p) => p,
        None => return Err(Error::new(ErrorKind::NotFound, "no input file")),
    };

    let text = read_to_string(path)?;

    for (line_number, token) in Lexer::lex(&text) {
        println!("#{line_number} {token}");
    }

    Ok(())
}
