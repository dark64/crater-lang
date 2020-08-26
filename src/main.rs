extern crate regex;

mod ast;
mod lexer;
mod parser;

use crate::lexer::Lexer;
use crate::parser::Parser;

fn main() {
    let code: &str = r#"
        #func main(a: int32, b: int32): string {
            let c: int32 = (a ** 2) + (b ** 2);

            if (a <= b) && ((c < b) || (c >= a)) && (a < 2) {
                print("hello {}!", "world");
            } else {
                print("bye world!");
            }

            let f: bool;
            f = a < b ? true : false;
            f = f && c > 10;

            return "peekaboo";
        }
    "#;

    let mut lexer = Lexer::new(code);
    let tokens = lexer.tokenize().unwrap();

    let function = Parser::new(tokens).parse();
    println!("\n{}", function);
}
