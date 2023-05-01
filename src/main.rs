use std::{fs::read_to_string, io::Error};
use lexer::Lexer;

mod string_table;
mod lexer;
fn main() -> Result<(), Error> {
    let path = "test.cl";
    let text = read_to_string(path)?;
    let mut lexer = Lexer::new(&text);

    println!("#name {path}");
    while let Some((line_number, token)) = lexer.lex(){
        lexer.print_token(line_number, token);
    }
    Ok(())
}