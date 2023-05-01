use lexer::Lexer;
use std::{fs::read_to_string, io::Error};

mod lexer;
mod string_table;
fn main() -> Result<(), Error> {
    let path = "test.cl";
    let text = read_to_string(path)?;
    let mut lexer = Lexer::new(&text);

    while let Some((line_number, token)) = lexer.lex() {
        println!("{}", lexer.token_to_string(line_number, token));
    }
    Ok(())
}
