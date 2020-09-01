use std::iter::Peekable;
use std::str::Chars;
use crate::lexer::tokens::{Token, TokenType};
use crate::common::Position;

pub mod tokens;

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    line: usize,
    col: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Self {
        Self {
            input: code.chars().peekable(),
            line: 1,
            col: 0,
        }
    }

    fn next_char(&mut self) -> Option<char> {
        if let Some(c) = self.input.next() {
            if self.is_linebreak(&c) {
                self.line += 1;
                self.col = 0;
            } else {
                self.col += 1;
            }
            Some(c)
        } else {
            None
        }
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.input.peek()
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.input.peek() {
            if c.is_whitespace() {
                let _ = self.next_char();
            } else {
                break;
            }
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();

        let mut value = String::with_capacity(10);
        let c = self.next_char()?;
        value.push(c);

        let position = self.get_position();
        let token_type = match c {
            '=' => {
                if let Some(&'=') = self.peek_char() {
                    value.push(self.next_char().unwrap());
                    Some(TokenType::Eq)
                } else {
                    Some(TokenType::Assign)
                }
            }
            '+' => {
                if let Some(&'+') = self.peek_char() {
                    value.push(self.next_char().unwrap());
                    Some(TokenType::Increment)
                } else {
                    Some(TokenType::Plus)
                }
            }
            '-' => {
                if let Some(n) = self.peek_char() {
                    if *n == '-' {
                        value.push(self.next_char().unwrap());
                        Some(TokenType::Decrement)
                    } else if n.is_ascii_digit() {
                        self.read_number(&mut value);
                        Some(TokenType::IntLiteral)
                    } else {
                        Some(TokenType::Minus)
                    }
                } else {
                    Some(TokenType::Minus)
                }
            }
            '*' => {
                if let Some(&'*') = self.peek_char() {
                    value.push(self.next_char().unwrap());
                    Some(TokenType::Power)
                } else {
                    Some(TokenType::Asterisk)
                }
            }
            '/' => {
                if let Some(&'/') = self.peek_char() {
                    while let Some(n) = self.next_char() {
                        if self.is_linebreak(&n) {
                            self.col = 0;
                            break;
                        }
                    }
                    Some(TokenType::Comment)
                } else {
                    Some(TokenType::Slash)
                }
            },
            '%' => Some(TokenType::Percent),
            '?' => Some(TokenType::QuestionMark),
            '&' => {
                if let Some(&'&') = self.peek_char() {
                    value.push(self.next_char().unwrap());
                    Some(TokenType::And)
                } else {
                    Some(TokenType::BitAnd)
                }
            },
            '|' => {
                if let Some(&'|') = self.peek_char() {
                    value.push(self.next_char().unwrap());
                    Some(TokenType::Or)
                } else {
                    Some(TokenType::BitOr)
                }
            },
            '^' => Some(TokenType::BitXor),
            '<' => {
                if let Some(&'=') = self.peek_char() {
                    value.push(self.next_char().unwrap());
                    Some(TokenType::Lte)
                } else {
                    Some(TokenType::Lt)
                }
            },
            '>' => {
                if let Some(&'=') = self.peek_char() {
                    value.push(self.next_char().unwrap());
                    Some(TokenType::Gte)
                } else {
                    Some(TokenType::Gt)
                }
            },
            '!' => {
                if let Some(&'=') = self.peek_char() {
                    value.push(self.next_char().unwrap());
                    Some(TokenType::Neq)
                } else {
                    Some(TokenType::Not)
                }
            },
            ',' => Some(TokenType::Comma),
            ';' => Some(TokenType::Semicolon),
            ':' => Some(TokenType::Colon),
            '(' => Some(TokenType::LParen),
            ')' => Some(TokenType::RParen),
            '{' => Some(TokenType::LBrace),
            '}' => Some(TokenType::RBrace),
            '[' => Some(TokenType::LBracket),
            ']' => Some(TokenType::RBracket),
            '"' => {
                value.pop(); // pop first quotation mark
                while let Some(n) = self.next_char() {
                    if n == '\\' && self.peek_char()? == &'"' { // allow escaped quotation marks
                        value.push(n);
                        value.push(self.next_char()?);
                    } else if n == '"' {
                        break; // end of string literal
                    } else {
                        value.push(n);
                    }
                }
                Some(TokenType::StrLiteral)
            }
            _ => {
                if c == '_' || c.is_ascii_alphabetic() {
                    self.read_identifier(&mut value);
                    Some(self.reserved_lookup(value.as_str()).unwrap_or(TokenType::Identifier))
                } else if c.is_ascii_digit() {
                    self.read_number(&mut value);
                    Some(TokenType::IntLiteral)
                } else {
                    None
                }
            },
        }?;
        if token_type == TokenType::Comment {
            return self.next_token();
        }
        Some(Token::new(token_type, value, position))
    }

    fn get_position(&self) -> Position {
        Position {
            line: self.line,
            col: self.col
        }
    }

    fn read_identifier(&mut self, value: &mut String) {
        while let Some(&c) = self.input.peek() {
            if c == '_' || c.is_ascii_alphabetic() || c.is_ascii_digit() {
                value.push(self.next_char().unwrap());
            } else {
                break;
            }
        }
    }

    fn read_number(&mut self, value: &mut String) {
        while let Some(&c) = self.input.peek() {
            if c.is_ascii_digit() {
                value.push(self.next_char().unwrap());
            } else {
                break;
            }
        }
    }

    fn reserved_lookup(&self, id: &str) -> Option<TokenType> {
        match id {
            "func" => Some(TokenType::Function),
            "let" => Some(TokenType::Let),
            "return" => Some(TokenType::Return),
            "if" => Some(TokenType::If),
            "else" => Some(TokenType::Else),
            "int32" => Some(TokenType::VarType),
            "uint32" => Some(TokenType::VarType),
            "bool" => Some(TokenType::VarType),
            "true" => Some(TokenType::BoolLiteral),
            "false" => Some(TokenType::BoolLiteral),
            _ => None
        }
    }

    fn is_linebreak(&self, c: &char) -> bool {
        *c == '\n'
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        self.next_token()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let mut lexer = Lexer::new(r#"
            // comment (should skip)
            _var0 var1 CONST_VAR 123 -123
            = + - * / % ** ++ -- == != < <= > >= ! ? & ^ | && || , ; : ( ) [ ] { }
            "string literal" "escaped \"string literal\""
            func let return if else int32 uint32 bool true false
        "#);

        while let Some(t) = lexer.next_token() {
            println!("{:?}", t);
        }
    }
}

// #[derive(Debug, Clone, PartialEq)]
// pub enum TokenType {
//     Keyword,
//     VarType,
//     Identifier,
//     IntLiteral,
//     StrLiteral,
//     BoolLiteral,
//     Comment,
//     Operator,
//     QuestionMark,
//     Assignment,
//     LParen,
//     RParen,
//     LBrace,
//     RBrace,
//     LBracket,
//     RBracket,
//     Hashtag,
//     Comma,
//     Semicolon,
//     Colon,
// }
//
// static TOKENS: [(TokenType, &'static str); 20] = [
//     (TokenType::Keyword, r'^\b(let|func|return|if|else)\b"),
//     (TokenType::VarType, r"^\b(u?int32|bool|string)\b"),
//     (TokenType::BoolLiteral, r"^\b(true|false)\b"),
//     (TokenType::IntLiteral, r"^\b[0-9]+\b"),
//     (TokenType::StrLiteral, r#"^"([^"\\]|\\.)*""#),
//     (TokenType::Identifier, r"^\b[a-zA-Z0-9_]+\b"),
//     (TokenType::Comment, r"^//.*"),
//     (
//         TokenType::Operator,
//         r"^(\+|-|\*\*?|/|%|==|!=|<=?|>=?|&&|\|\||!)",
//     ),
//     (TokenType::QuestionMark, r"^\?"),
//     (TokenType::Assignment, r"^="),
//     (TokenType::LParen, r"^\("),
//     (TokenType::RParen, r"^\)"),
//     (TokenType::LBracket, r"^\["),
//     (TokenType::RBracket, r"^\]"),
//     (TokenType::LBrace, r"^\{"),
//     (TokenType::RBrace, r"^\}"),
//     (TokenType::Hashtag, r"^#"),
//     (TokenType::Comma, r"^,"),
//     (TokenType::Semicolon, r"^;"),
//     (TokenType::Colon, r"^:"),
// ];
//
// #[derive(Debug, Clone)]
// pub struct Token {
//     pub ty: TokenType,
//     pub value: String,
// }
//
// impl Token {
//     pub fn new(token_type: TokenType, value: String) -> Self {
//         Self {
//             ty: token_type,
//             value,
//         }
//     }
// }
//
// pub struct Lexer<'a> {
//     code: &'a str,
//     regex_cache: HashMap<&'a str, Regex>,
// }
//
// impl<'a> Lexer<'a> {
//     pub fn new(code: &'a str) -> Self {
//         Self {
//             code: code.trim_start(),
//             regex_cache: HashMap::new(),
//         }
//     }
//
//     pub fn tokenize(&mut self) -> Result<Vec<Token>, Error> {
//         let mut tokens: Vec<Token> = Vec::new();
//         while !self.code.is_empty() {
//             let token = self.tokenize_single()?;
//             if token.ty != TokenType::Comment {
//                 tokens.push(token);
//             }
//             self.code = self.code.trim_start();
//         }
//         Ok(tokens)
//     }
//
//     fn tokenize_single(&mut self) -> Result<Token, Error> {
//         for (token_type, pattern) in TOKENS.iter() {
//             let regex = self
//                 .regex_cache
//                 .entry(pattern)
//                 .or_insert(Regex::new(pattern).unwrap());
//             if let Some(caps) = regex.captures(self.code) {
//                 self.code = &self.code[caps[0].len()..];
//                 return Ok(Token::new(token_type.clone(), String::from(&caps[0])));
//             }
//         }
//         Err(Error::UnknownToken)
//     }
// }
