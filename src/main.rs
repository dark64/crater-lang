extern crate regex;

mod ast;
mod lexer;
mod parser;

use crate::lexer::Lexer;
use crate::parser::Parser;

fn main() {
    let code: &str = r#"
        func main(a: int32, b: int32) {
            return a + b;
        }
    "#;

    let mut lexer = Lexer::new(code);
    let tokens = lexer.tokenize().unwrap();

    tokens.iter().for_each(|t| println!("{:?}", t));

    Parser::new(tokens).parse();
}
