use crate::lexer::Lexer;
use crate::parser::Parser;

mod common;
mod ast;
mod lexer;
mod parser;

fn main() {
    let code: &str = r#"
        func test(a: int32): int32 {
            return a;
        }

        func main(a: int32): int32 {
            return test(a);
        }

        let b: int32 = main(3);
        b = 4;
        return b;
    "#;

    let lexer = Lexer::new(code);
    let statements = Parser::new(lexer).parse()
        .unwrap_or_else(|e| {
            println!("({}) {}", e.pos, e);
            std::process::exit(1);
        });

    for s in statements {
        print!("{}", s);
    }
}
