use lexer::Lexer;
use parser::Parser;
use string_table::StrTable;
use std::{
    fs::read_to_string,
    io::{Error, ErrorKind},
};

mod ast;
mod lexer;
mod parser;
mod semant;
mod string_table;
fn main() -> Result<(), Error> {
    let path = match std::env::args().nth(1) {
        Some(p) => p,
        None => return Err(Error::new(ErrorKind::NotFound, "no input file")),
    };

    let text = read_to_string(path)?;

    let mut str_table = StrTable::new();
    let tokens = Lexer::lex(&text, &mut str_table).collect();
    let mut parser = Parser::new(tokens, &mut str_table);

    let ast = parser.parse_program().unwrap();

    println!("{ast}");
    Ok(())
}
