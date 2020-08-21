use crate::ast::types::TypeNode;
use crate::ast::{
    Assignee, AssigneeNode, Expression, ExpressionNode, Function, FunctionNode, ParameterNode,
    Statement, StatementNode, Variable, VariableNode,
};
use crate::lexer::{Token, TokenType};
use core::fmt;
use std::convert::TryInto;

#[derive(Debug, Clone)]
pub struct Position {
    pub line: usize,
    pub col: usize,
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

pub struct Parser {
    tokens: Vec<Token>,
}

impl<'ast> Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens }
    }

    pub fn parse(&mut self) -> FunctionNode {
        self.consume_function() // TODO: should return module
    }

    fn consume_function(&mut self) -> FunctionNode {
        let public = self.peek(TokenType::Hashtag);
        let token = if public {
            self.eat(TokenType::Hashtag);
            self.eat(TokenType::Keyword)
        } else {
            self.eat(TokenType::Keyword)
        };

        match token.value.as_str() {
            "func" => {
                let identifier = self.eat(TokenType::Identifier);
                let parameters = self.consume_function_params();
                self.eat(TokenType::Colon);

                let return_type = self.eat(TokenType::VarType);
                let statements = self.consume_block();

                self.eat(TokenType::RBrace);
                let function = Function::new(
                    identifier.value,
                    parameters,
                    statements,
                    TypeNode::from(return_type.value.try_into().unwrap()),
                    public,
                );
                FunctionNode::from(function)
            }
            _ => panic!("Invalid function definition"),
        }
    }

    fn consume_block(&mut self) -> Vec<StatementNode> {
        let mut statements: Vec<StatementNode> = vec![];
        self.eat(TokenType::LBrace);

        if self.peek(TokenType::RBrace) {
            self.eat(TokenType::RBrace);
        } else {
            while !self.peek(TokenType::RBrace) {
                statements.push(self.consume_statement());
            }
        }
        statements
    }

    fn consume_statement(&mut self) -> StatementNode {
        let token = self.eat_unchecked();
        match token.ty {
            TokenType::Keyword => match token.value.as_str() {
                "let" => {
                    let var = self.consume_variable_definition();
                    self.eat(TokenType::Semicolon);
                    StatementNode::from(Statement::Declaration(var))
                }
                "return" => {
                    let expr = self.consume_expression();
                    self.eat(TokenType::Semicolon);
                    StatementNode::from(Statement::Return(expr))
                }
                _ => panic!("Invalid keyword in statement {:?}", token),
            },
            TokenType::Identifier => {
                self.eat(TokenType::Assignment);
                let expr = self.consume_expression();
                let assignee = AssigneeNode::from(Assignee::Identifier(token.value));
                self.eat(TokenType::Semicolon);
                StatementNode::from(Statement::Definition(assignee, expr))
            }
            _ => panic!("Invalid statement {:?}", token),
        }
    }

    fn consume_expression(&mut self) -> ExpressionNode {
        let token = self.eat_unchecked();
        let left = match token.ty {
            TokenType::BoolLiteral => {
                ExpressionNode::from(Expression::BooleanConstant(token.value.eq("true")))
            }
            TokenType::IntLiteral => ExpressionNode::from(Expression::Int32Constant(
                token.value.parse::<i32>().unwrap(),
            )),
            TokenType::Identifier => {
                if self.peek(TokenType::LParen) {
                    ExpressionNode::from(Expression::FunctionCall(
                        token.value,
                        self.consume_function_args(),
                    ))
                } else {
                    ExpressionNode::from(Expression::Identifier(token.value))
                }
            }
            TokenType::LParen => {
                let expression = self.consume_expression();
                self.eat(TokenType::RParen);
                expression
            }
            _ => panic!("Unexpected token {:?}", token),
        };

        if self.peek(TokenType::Operator) {
            let operator = self.eat(TokenType::Operator);
            let right = self.consume_expression();

            let expression = match operator.value.as_str() {
                "+" => Expression::Add(Box::new(left), Box::new(right)),
                "-" => Expression::Subtract(Box::new(left), Box::new(right)),
                "*" => Expression::Multiply(Box::new(left), Box::new(right)),
                "/" => Expression::Divide(Box::new(left), Box::new(right)),
                _ => unreachable!(),
            };
            ExpressionNode::from(expression)
        } else {
            left
        }
    }

    fn consume_variable_definition(&mut self) -> VariableNode {
        let identifier = self.eat(TokenType::Identifier);
        self.eat(TokenType::Colon);
        let ty = self.eat(TokenType::VarType);
        VariableNode::from(Variable::new(
            identifier.value,
            TypeNode::from(ty.value.try_into().unwrap()),
        ))
    }

    fn consume_function_params(&mut self) -> Vec<ParameterNode> {
        let mut parameters: Vec<ParameterNode> = vec![];
        self.eat(TokenType::LParen);

        if self.peek(TokenType::Identifier) {
            let var = self.consume_variable_definition();
            parameters.push(var);

            while self.peek(TokenType::Comma) {
                self.eat(TokenType::Comma);
                parameters.push(self.consume_variable_definition());
            }
            self.eat(TokenType::RParen);
        } else {
            self.eat(TokenType::RParen);
        }

        parameters
    }

    fn consume_function_args(&mut self) -> Vec<ExpressionNode> {
        let mut expressions: Vec<ExpressionNode> = vec![];
        self.eat(TokenType::LParen);

        if self.peek(TokenType::RParen) {
            self.eat(TokenType::RParen);
        } else {
            expressions.push(self.consume_expression());
            while self.peek(TokenType::Comma) {
                self.eat(TokenType::Comma);
                expressions.push(self.consume_expression());
            }
            self.eat(TokenType::RParen);
        }

        expressions
    }

    fn eat_unchecked(&mut self) -> Token {
        self.tokens.remove(0)
    }

    fn eat(&mut self, expected_type: TokenType) -> Token {
        let token = self.tokens.remove(0);
        if token.ty == expected_type {
            token
        } else {
            panic!(
                "Expected token type {:?}, but got {:?}",
                expected_type, token
            );
        }
    }

    fn peek(&self, expected_type: TokenType) -> bool {
        self.tokens.get(0).unwrap().ty == expected_type
    }
}
