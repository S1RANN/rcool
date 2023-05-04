use lexer::Lexer;
use std::{
    fs::read_to_string,
    io::{Error, ErrorKind},
};

mod lexer;
mod string_table;
fn main() -> Result<(), Error> {
    let path = match std::env::args().nth(1) {
        Some(p) => p,
        None => return Err(Error::new(ErrorKind::NotFound, "no input file")),
    };
    
    let text = read_to_string(path)?;
    let mut lexer = Lexer::new(&text);

    for (line_number, token) in  lexer.lex() {
        println!("#{} {}", line_number, token);
    }

    Ok(())
}
