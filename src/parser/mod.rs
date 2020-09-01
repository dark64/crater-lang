use crate::ast::types::{Type, TypeError};
use crate::ast::{
    Assignee, AssigneeNode, Expression, ExpressionNode, Function, FunctionNode,
    ParameterNode, Statement, StatementNode, Variable, VariableNode,
};
use crate::lexer::{Lexer};
use std::convert::{TryInto};
use std::iter::Peekable;
use crate::lexer::tokens::{Token, TokenType};
use crate::common::Position;
use core::fmt;
use std::fmt::Formatter;

pub enum ParserErrorKind {
    UnexpectedTokenType(TokenType, TokenType),
    UnexpectedToken(Token),
    UnexpectedEof,
    InvalidType(TypeError)
}

pub struct ParserError {
    pub kind: ParserErrorKind,
    pub pos: Position
}

impl ParserError {
    fn new(kind: ParserErrorKind, pos: Position) -> Self {
        Self { kind, pos }
    }
}

impl fmt::Display for ParserErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ParserErrorKind::UnexpectedTokenType(ety, ty) => write!(f, "expected token of type {:?}, but got {:?}", ety, ty),
            ParserErrorKind::UnexpectedToken(token) => write!(f, "unexpected token {}", token),
            ParserErrorKind::UnexpectedEof => writeln!(f, "unexpected EOF"),
            ParserErrorKind::InvalidType(err) => write!(f, "{}", err)
        }
    }
}


impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "ParseError: {}", self.kind)
    }
}

pub struct Parser<'ast> {
    iter: Peekable<Lexer<'ast>>,
}

impl<'ast> Parser<'ast> {
    pub fn new(lexer: Lexer<'ast>) -> Self {
        Self {
            iter: lexer.into_iter().peekable()
        }
    }

    pub fn parse(&mut self) -> Result<Vec<StatementNode>, ParserError> {
        let mut statements: Vec<StatementNode> = vec![];
        while let Some(s) = self.consume_statement()? {
            statements.push(s);
        }
        Ok(statements)
    }

    fn consume_function(&mut self, pos: Position) -> Result<FunctionNode, ParserError> {
        let identifier = self.consume(TokenType::Identifier)?;
        let parameters = self.consume_function_params()?;
        let returns = if self.peek(TokenType::Colon) {
            self.consume(TokenType::Colon)?;
            let return_type = self.consume(TokenType::VarType)?;
            Some(return_type.value.try_into().unwrap())
        } else {
            None
        };

        let statements = self.consume_block()?;
        if self.peek(TokenType::Semicolon) {
            self.consume(TokenType::Semicolon)?;
        }
        Ok(FunctionNode::from(Function::new(
            identifier.value,
            parameters,
            statements,
            returns
        ), pos))
    }

    fn consume_block(&mut self) -> Result<Vec<StatementNode>, ParserError> {
        let mut statements: Vec<StatementNode> = vec![];
        self.consume(TokenType::LBrace)?;
        while !self.peek(TokenType::RBrace) {
            match self.consume_statement()? {
                Some(s) => statements.push(s),
                None => break
            }
        }
        self.consume(TokenType::RBrace)?;
        Ok(statements)
    }

    fn consume_statement(&mut self) -> Result<Option<StatementNode>, ParserError> {
        if self.peek_unchecked().is_none() {
            return Ok(None);
        }

        let token = self.consume_unchecked()?;
        let pos = token.pos.clone();

        let statement = match token.ty {
            TokenType::Function => Ok(Statement::FunctionDef(self.consume_function(token.pos.clone())?)),
            TokenType::Let => {
                let var = self.consume_variable_definition()?;
                let statement = if self.peek(TokenType::Assign) {
                    self.consume(TokenType::Assign)?;
                    Statement::Definition(var, self.consume_expression()?)
                } else {
                    Statement::Declaration(var)
                };
                self.consume(TokenType::Semicolon)?;
                Ok(statement)
            },
            TokenType::Return => {
                let expr = self.consume_expression()?;
                self.consume(TokenType::Semicolon)?;
                Ok(Statement::Return(expr))
            },
            TokenType::If => {
                let condition = self.consume_expression()?;
                let statements = self.consume_block()?;
                let alternative = if self.peek(TokenType::Else) {
                    self.consume(TokenType::Else)?;
                    Some(self.consume_block()?)
                } else {
                    None
                };
                Ok(Statement::Condition(condition, statements, alternative))
            },
            TokenType::Identifier => {
                let statement = if self.peek(TokenType::LParen) {
                    Statement::FunctionCall(token.value, self.consume_function_args()?)
                } else {
                    self.consume(TokenType::Assign)?;
                    let expr = self.consume_expression()?;
                    let assignee = AssigneeNode::from(Assignee::Identifier(token.value), pos.clone());
                    Statement::Assignment(assignee, expr)
                };
                self.consume(TokenType::Semicolon)?;
                Ok(statement)
            }
            _ => Err(ParserError::new(ParserErrorKind::UnexpectedToken(token.clone()), pos)),
        }?;
        Ok(Some(StatementNode::from(statement, pos)))
    }

    // fn consume_basic_expression(&mut self) -> ExpressionNode {
    //     let token = self.eat_unchecked();
    //     let left = match token.ty {
    //         TokenType::BoolLiteral => {
    //             ExpressionNode::from(Expression::BooleanConstant(token.value.eq("true")))
    //         }
    //         TokenType::IntLiteral => ExpressionNode::from(Expression::Int32Constant(
    //             token.value.parse::<i32>().unwrap(),
    //         )),
    //         TokenType::StrLiteral => ExpressionNode::from(Expression::StringConstant(token.value)),
    //         TokenType::Identifier => {
    //             if self.peek(TokenType::LParen) {
    //                 ExpressionNode::from(Expression::FunctionCall(
    //                     token.value,
    //                     self.consume_function_args(),
    //                 ))
    //             } else {
    //                 ExpressionNode::from(Expression::Identifier(token.value))
    //             }
    //         }
    //         TokenType::Operator => match token.value.as_str() {
    //             "!" => ExpressionNode::from(Expression::Not(Box::new(self.consume_expression()))),
    //             _ => panic!("Unexpected operator token {:?}", token),
    //         },
    //         TokenType::LParen => {
    //             let expression = self.consume_expression();
    //             self.eat(TokenType::RParen);
    //             expression
    //         }
    //         _ => panic!("Unexpected token {:?}", token),
    //     };
    //
    //     if self.peek(TokenType::Operator) {
    //         let operator = self.eat(TokenType::Operator);
    //         let right = self.consume_basic_expression();
    //
    //         ExpressionNode::from(match operator.value.as_str() {
    //             "+" => Expression::Add(Box::new(left), Box::new(right)),
    //             "-" => Expression::Sub(Box::new(left), Box::new(right)),
    //             "*" => Expression::Mul(Box::new(left), Box::new(right)),
    //             "/" => Expression::Div(Box::new(left), Box::new(right)),
    //             "**" => Expression::Pow(Box::new(left), Box::new(right)),
    //             "%" => Expression::Mod(Box::new(left), Box::new(right)),
    //             "==" => Expression::Eq(Box::new(left), Box::new(right)),
    //             "!=" => Expression::Neq(Box::new(left), Box::new(right)),
    //             "<" => Expression::Lt(Box::new(left), Box::new(right)),
    //             "<=" => Expression::Le(Box::new(left), Box::new(right)),
    //             ">=" => Expression::Ge(Box::new(left), Box::new(right)),
    //             ">" => Expression::Gt(Box::new(left), Box::new(right)),
    //             "&&" => Expression::And(Box::new(left), Box::new(right)),
    //             "||" => Expression::Or(Box::new(left), Box::new(right)),
    //             _ => unreachable!(),
    //         })
    //     } else {
    //         left
    //     }
    // }

    fn consume_expression(&mut self) -> Result<ExpressionNode, ParserError> {
        let token = self.consume_unchecked()?;
        let pos = token.pos.clone();

        let expression = match token.ty {
            TokenType::BoolLiteral => Ok(Expression::BooleanConstant(token.value.eq("true"))),
            TokenType::IntLiteral => Ok(Expression::Int32Constant(token.value.parse::<i32>().unwrap())),
            TokenType::StrLiteral => Ok(Expression::StringConstant(token.value)),
            TokenType::Identifier => {
                if self.peek(TokenType::LParen) {
                    Ok(Expression::FunctionCall(token.value, self.consume_function_args()?))
                } else {
                    Ok(Expression::Identifier(token.value))
                }
            },
            _ => Err(ParserError::new(ParserErrorKind::UnexpectedToken(token.clone()), pos)),
        }?;

        // TODO: implement complex expressions
        Ok(ExpressionNode::from(expression, pos))
    }

    fn consume_variable_definition(&mut self) -> Result<VariableNode, ParserError> {
        let identifier = self.consume(TokenType::Identifier)?;
        self.consume(TokenType::Colon)?;
        let var_type = self.consume(TokenType::VarType)?;

        Ok(VariableNode::from(
            Variable::new(identifier.value, var_type.value.try_into().unwrap()),
            identifier.pos
        ))
    }

    fn consume_function_params(&mut self) -> Result<Vec<ParameterNode>, ParserError> {
        let mut parameters: Vec<ParameterNode> = vec![];
        self.consume(TokenType::LParen)?;

        if self.peek(TokenType::Identifier) {
            let var = self.consume_variable_definition()?;
            parameters.push(var);

            while self.peek(TokenType::Comma) {
                self.consume(TokenType::Comma)?;
                parameters.push(self.consume_variable_definition()?);
            }
        }

        self.consume(TokenType::RParen)?;
        Ok(parameters)
    }

    fn consume_function_args(&mut self) -> Result<Vec<ExpressionNode>, ParserError> {
        let mut expressions: Vec<ExpressionNode> = vec![];
        self.consume(TokenType::LParen)?;

        if !self.peek(TokenType::RParen) {
            expressions.push(self.consume_expression()?);
            while self.peek(TokenType::Comma) {
                self.consume(TokenType::Comma)?;
                expressions.push(self.consume_expression()?);
            }
        }
        self.consume(TokenType::RParen)?;
        Ok(expressions)
    }

    fn consume_unchecked(&mut self) -> Result<Token, ParserError> {
        let token = self.iter.next().ok_or(ParserError::new(ParserErrorKind::UnexpectedEof, Position::default()))?;
        Ok(token)
    }

    fn consume(&mut self, expected_type: TokenType) -> Result<Token, ParserError> {
        let token = self.iter.next().ok_or(ParserError::new(ParserErrorKind::UnexpectedEof, Position::default()))?;
        if token.ty == expected_type {
            Ok(token)
        } else {
            Err(ParserError::new(ParserErrorKind::UnexpectedTokenType(expected_type, token.ty), token.pos))
        }
    }

    fn peek_unchecked(&mut self) -> Option<&Token> {
        self.iter.peek()
    }

    fn peek_next(&mut self, expected_type: TokenType) -> Option<&Token> {
        let token = self.iter.peek()?;
        if token.ty == expected_type {
            Some(token)
        } else {
            None
        }
    }

    fn peek(&mut self, expected_type: TokenType) -> bool {
        if let Some(t) = self.iter.peek() {
            t.ty == expected_type
        } else {
            false
        }
    }
}
