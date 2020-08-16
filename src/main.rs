extern crate regex;

mod lexer;
use crate::lexer::Lexer;

fn main() {
    let code: &str = r#"
        include "a.cl"
        include "b.cl"

        func main(a: int32, b: int32) {
            return a + b;
        }
    "#;

    let mut lexer = Lexer::new(code);
    let tokens = lexer.tokenize().unwrap();

    tokens.iter()
        .for_each(|t| println!("{:?}", t));
}
