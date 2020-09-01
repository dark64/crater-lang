use crate::common::Position;
use core::fmt;
use std::fmt::Formatter;

#[derive(Debug, Clone)]
pub struct Token {
    pub ty: TokenType,
    pub value: String,
    pub pos: Position,
}

impl Token {
    pub fn new(ty: TokenType, value: String, pos: Position) -> Token {
        Token { ty, value, pos }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Token(type={:?}, value=\"{}\")", self.ty, self.value)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenType {
    Identifier,  // ^([_a-zA-Z][_a-zA-Z0-9]*)
    VarType,     // ^(u?int32|string|bool)

    // Keywords
    Function,    // func
    Let,         // let
    Return,      // return
    If,          // if
    Else,        // else

    // Literals
    IntLiteral,  // -?[0-9]+
    StrLiteral,  // ^"([^"\\]|\\.)*"
    BoolLiteral, // ^(true|false)

    // Operators
    Assign,       // =
    Plus,         // +
    Minus,        // -
    Asterisk,     // *
    Slash,        // /
    Percent,      // %
    QuestionMark, // ?
    Increment,    // ++
    Decrement,    // --
    Power,        // **

    // Bitwise operators
    BitAnd,      // &
    BitOr,       // |
    BitXor,      // ^

    // Logical operators
    Lt,          // <
    Lte,         // <=
    Gt,          // >
    Gte,         // >=
    Eq,          // ==
    Neq,         // !=
    And,         // &&
    Or,          // ||
    Not,         // !

    // Delimiters
    Comma,       // ,
    Semicolon,   // ;
    Colon,       // :

    // P
    LParen,      // (
    RParen,      // )
    LBrace,      // {
    RBrace,      // }
    LBracket,    // [
    RBracket,    // ]

    Comment,     // single line comment -> //.*\n?
}