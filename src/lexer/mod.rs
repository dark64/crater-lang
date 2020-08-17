use core::fmt;
use regex::Regex;
use std::collections::HashMap;

#[derive(Debug)]
pub enum Error {
    UnknownToken,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::UnknownToken => write!(f, "Unknown token"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TokenType {
    Keyword,
    VarType,
    Identifier,
    IntLiteral,
    StrLiteral,
    Operator,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Semicolon,
    Colon,
}

static TOKENS: [(TokenType, &'static str); 15] = [
    (TokenType::Keyword, r"^\b(func|return)\b"),
    (TokenType::VarType, r"^\b(u?int(?:8|16|32|64)|bool)\b"),
    (TokenType::Identifier, r"^\b[a-zA-Z0-9_]+\b"),
    (TokenType::IntLiteral, r"^\b[0-9]+\b"),
    (TokenType::StrLiteral, r#"^"([^"\\]|\\.)*""#),
    (TokenType::Operator, r"^(\+|-|/|\*)"),
    (TokenType::LParen, r"^\("),
    (TokenType::RParen, r"^\)"),
    (TokenType::LBracket, r"^\["),
    (TokenType::RBracket, r"^\]"),
    (TokenType::LBrace, r"^\{"),
    (TokenType::RBrace, r"^\}"),
    (TokenType::Comma, r"^,"),
    (TokenType::Semicolon, r"^:"),
    (TokenType::Colon, r"^;"),
];

#[derive(Debug, Clone)]
pub struct Token {
    token_type: TokenType,
    value: String,
}

impl Token {
    pub fn new(token_type: TokenType, value: String) -> Self {
        Self { token_type, value }
    }
}

pub struct Lexer<'a> {
    code: &'a str,
    regex_cache: HashMap<&'a str, Regex>,
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Self {
        Self {
            code: code.trim_start(),
            regex_cache: HashMap::new(),
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, Error> {
        let mut tokens: Vec<Token> = Vec::new();
        while !self.code.is_empty() {
            tokens.push(self.tokenize_single()?);
            self.code = self.code.trim_start();
        }
        Ok(tokens)
    }

    fn tokenize_single(&mut self) -> Result<Token, Error> {
        for (token_type, pattern) in TOKENS.iter() {
            let regex = self
                .regex_cache
                .entry(pattern)
                .or_insert(Regex::new(pattern).unwrap());
            if let Some(caps) = regex.captures(self.code) {
                self.code = &self.code[caps[0].len()..];
                return Ok(Token::new(token_type.clone(), String::from(&caps[0])));
            }
        }
        Err(Error::UnknownToken)
    }
}
