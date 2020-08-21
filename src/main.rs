extern crate regex;

mod ast;
mod lexer;
mod parser;

use crate::lexer::Lexer;
use crate::parser::Parser;

fn main() {
    let code: &str = r#"
        #func main(a: int32, b: int32): bool {
            let c: int32 = (a ** 2) + (b * b);
            c = sqrt(c);
            return (c > 10) ? true : false;
        }
    "#;

    let mut lexer = Lexer::new(code);
    let tokens = lexer.tokenize().unwrap();

    let function = Parser::new(tokens).parse();
    println!("\n{}", function);
}
