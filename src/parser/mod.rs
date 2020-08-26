use crate::ast::types::{Type, TypeNode};
use crate::ast::{
    Assignee, AssigneeNode, Expression, ExpressionNode, Function, FunctionNode, FunctionSignature,
    ParameterNode, Statement, StatementNode, Variable, VariableNode,
};
use crate::lexer::{Token, TokenType};
use core::fmt;
use std::convert::{TryFrom, TryInto};

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
        if public {
            self.eat(TokenType::Hashtag);
        }
        let token = self.eat(TokenType::Keyword);
        match token.value.as_str() {
            "func" => {
                let identifier = self.eat(TokenType::Identifier);
                let parameters = self.consume_function_params();

                let returns = if self.peek(TokenType::Colon) {
                    self.eat(TokenType::Colon);
                    let return_type = self.eat(TokenType::VarType);
                    Some(TypeNode::from(Type::try_from(return_type.value).unwrap()))
                } else {
                    None
                };

                let statements = self.consume_block();
                let signature = FunctionSignature {
                    inputs: parameters
                        .iter()
                        .map(|p| p.value.ty.value.clone())
                        .collect(),
                    output: returns.clone().map(|p| p.value),
                };

                let function = Function::new(
                    identifier.value,
                    parameters,
                    statements,
                    returns,
                    signature,
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
        while !self.peek(TokenType::RBrace) {
            statements.push(self.consume_statement());
        }
        self.eat(TokenType::RBrace);
        statements
    }

    fn consume_statement(&mut self) -> StatementNode {
        let token = self.eat_unchecked();
        match token.ty {
            TokenType::Keyword => match token.value.as_str() {
                "let" => {
                    let var = self.consume_variable_definition();
                    let statement = if self.peek(TokenType::Assignment) {
                        self.eat(TokenType::Assignment);
                        StatementNode::from(Statement::Definition(
                            Box::new(var),
                            Box::new(self.consume_expression()),
                        ))
                    } else {
                        StatementNode::from(Statement::Declaration(Box::new(var)))
                    };
                    self.eat(TokenType::Semicolon);
                    statement
                }
                "return" => {
                    let expr = self.consume_expression();
                    self.eat(TokenType::Semicolon);
                    StatementNode::from(Statement::Return(Box::new(expr)))
                }
                "if" => {
                    let condition = self.consume_expression();
                    let statements = self.consume_block();
                    let alternative = match self.peek_next(TokenType::Keyword) {
                        Some(keyword) => match keyword.value.as_str() {
                            "else" => {
                                self.eat(TokenType::Keyword);
                                Some(Box::new(self.consume_block()))
                            }
                            _ => None,
                        },
                        _ => None,
                    };
                    StatementNode::from(Statement::Condition(
                        Box::new(condition),
                        Box::new(statements),
                        alternative,
                    ))
                }
                _ => panic!("Invalid keyword in statement {:?}", token),
            },
            TokenType::Identifier => {
                let statement = if self.peek(TokenType::LParen) {
                    Statement::FunctionCall(token.value, self.consume_function_args())
                } else {
                    self.eat(TokenType::Assignment);
                    let expr = self.consume_expression();
                    let assignee = AssigneeNode::from(Assignee::Identifier(token.value));
                    Statement::Assignment(Box::new(assignee), Box::new(expr))
                };
                self.eat(TokenType::Semicolon);
                StatementNode::from(statement)
            }
            _ => panic!("Invalid statement {:?}", token),
        }
    }

    fn consume_basic_expression(&mut self) -> ExpressionNode {
        let token = self.eat_unchecked();
        let left = match token.ty {
            TokenType::BoolLiteral => {
                ExpressionNode::from(Expression::BooleanConstant(token.value.eq("true")))
            }
            TokenType::IntLiteral => ExpressionNode::from(Expression::Int32Constant(
                token.value.parse::<i32>().unwrap(),
            )),
            TokenType::StrLiteral => ExpressionNode::from(Expression::StringConstant(token.value)),
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
            TokenType::Operator => match token.value.as_str() {
                "!" => ExpressionNode::from(Expression::Not(Box::new(self.consume_expression()))),
                _ => panic!("Unexpected operator token {:?}", token),
            },
            TokenType::LParen => {
                let expression = self.consume_expression();
                self.eat(TokenType::RParen);
                expression
            }
            _ => panic!("Unexpected token {:?}", token),
        };

        if self.peek(TokenType::Operator) {
            let operator = self.eat(TokenType::Operator);
            let right = self.consume_basic_expression();

            ExpressionNode::from(match operator.value.as_str() {
                "+" => Expression::Add(Box::new(left), Box::new(right)),
                "-" => Expression::Sub(Box::new(left), Box::new(right)),
                "*" => Expression::Mul(Box::new(left), Box::new(right)),
                "/" => Expression::Div(Box::new(left), Box::new(right)),
                "**" => Expression::Pow(Box::new(left), Box::new(right)),
                "%" => Expression::Mod(Box::new(left), Box::new(right)),
                "==" => Expression::Eq(Box::new(left), Box::new(right)),
                "!=" => Expression::Neq(Box::new(left), Box::new(right)),
                "<" => Expression::Lt(Box::new(left), Box::new(right)),
                "<=" => Expression::Le(Box::new(left), Box::new(right)),
                ">=" => Expression::Ge(Box::new(left), Box::new(right)),
                ">" => Expression::Gt(Box::new(left), Box::new(right)),
                "&&" => Expression::And(Box::new(left), Box::new(right)),
                "||" => Expression::Or(Box::new(left), Box::new(right)),
                _ => unreachable!(),
            })
        } else {
            left
        }
    }

    fn consume_expression(&mut self) -> ExpressionNode {
        let expression = self.consume_basic_expression();
        if self.peek(TokenType::QuestionMark) {
            self.eat(TokenType::QuestionMark);
            let consequence = self.consume_expression();
            self.eat(TokenType::Colon);
            let alternative = self.consume_expression();

            ExpressionNode::from(Expression::Ternary(
                Box::new(expression),
                Box::new(consequence),
                Box::new(alternative),
            ))
        } else {
            expression
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
        }

        self.eat(TokenType::RParen);
        parameters
    }

    fn consume_function_args(&mut self) -> Vec<ExpressionNode> {
        let mut expressions: Vec<ExpressionNode> = vec![];
        self.eat(TokenType::LParen);

        if !self.peek(TokenType::RParen) {
            expressions.push(self.consume_expression());
            while self.peek(TokenType::Comma) {
                self.eat(TokenType::Comma);
                expressions.push(self.consume_expression());
            }
        }
        self.eat(TokenType::RParen);
        expressions
    }

    fn eat_unchecked(&mut self) -> Token {
        self.tokens.remove(0)
    }

    fn eat(&mut self, expected_type: TokenType) -> Token {
        let token = self.tokens.remove(0);
        if token.ty == expected_type {
            println!("{:?}", token);
            token
        } else {
            panic!(
                "Expected token type {:?}, but got {:?}",
                expected_type, token
            );
        }
    }

    fn peek_next(&self, expected_type: TokenType) -> Option<&Token> {
        let token = self.tokens.get(0).unwrap();
        return if token.ty == expected_type {
            Some(token)
        } else {
            None
        };
    }

    fn peek(&self, expected_type: TokenType) -> bool {
        self.tokens.get(0).unwrap().ty == expected_type
    }
}
