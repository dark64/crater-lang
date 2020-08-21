use crate::ast::node::Node;
use crate::ast::types::{TypeNode};
use core::fmt;
// use std::collections::HashMap;

pub mod node;
pub mod types;

pub type Identifier = String;
pub type FunctionIdentifier = Identifier;

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(Identifier),
    Int32Constant(i32),
    BooleanConstant(bool),
    Add(Box<ExpressionNode>, Box<ExpressionNode>),
    Subtract(Box<ExpressionNode>, Box<ExpressionNode>),
    Multiply(Box<ExpressionNode>, Box<ExpressionNode>),
    Divide(Box<ExpressionNode>, Box<ExpressionNode>),
    FunctionCall(FunctionIdentifier, Vec<ExpressionNode>),
}

pub type ExpressionNode = Node<Expression>;

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Expression::Int32Constant(ref i) => write!(f, "{}", i),
            Expression::Identifier(ref identifier) => write!(f, "{}", identifier),
            Expression::BooleanConstant(b) => write!(f, "{}", b),
            Expression::Add(ref left, ref right) => write!(f, "({} + {})", left, right),
            Expression::Subtract(ref lhs, ref rhs) => write!(f, "({} - {})", lhs, rhs),
            Expression::Multiply(ref lhs, ref rhs) => write!(f, "({} * {})", lhs, rhs),
            Expression::Divide(ref lhs, ref rhs) => write!(f, "({} / {})", lhs, rhs),
            Expression::FunctionCall(ref i, ref p) => {
                write!(f, "{}(", i)?;
                for (i, param) in p.iter().enumerate() {
                    write!(f, "{}", param)?;
                    if i < p.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub id: Identifier,
    pub ty: TypeNode,
}

impl Variable {
    pub fn new(id: Identifier, ty: TypeNode) -> Self {
        Self { id, ty }
    }
}

pub type VariableNode = Node<Variable>;

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.id, self.ty)
    }
}

#[derive(Debug, Clone)]
pub enum Assignee {
    Identifier(Identifier),
}

pub type AssigneeNode = Node<Assignee>;

impl fmt::Display for Assignee {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Assignee::Identifier(ref var) => write!(f, "{}", var),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Return(ExpressionNode),
    Declaration(VariableNode),
    Definition(AssigneeNode, ExpressionNode),
}

pub type StatementNode = Node<Statement>;

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Statement::Return(ref expr) => write!(f, "\treturn {};", expr),
            Statement::Declaration(ref var) => write!(f, "\tlet {};", var),
            Statement::Definition(ref assignee, ref expr) => write!(f, "\t{} = {};", assignee, expr),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub inputs: Vec<TypeNode>,
    pub output: TypeNode,
}

pub type ParameterNode = VariableNode;

#[derive(Debug, Clone)]
pub struct Function {
    pub identifier: Identifier,
    pub parameters: Vec<ParameterNode>,
    pub statements: Vec<StatementNode>,
    pub returns: TypeNode,
    // pub signature: FunctionSignature,
    pub public: bool,
}

impl Function {
    pub fn new(identifier: Identifier,
               parameters: Vec<ParameterNode>,
               statements: Vec<StatementNode>,
               returns: TypeNode,
               public: bool) -> Self {
        Self { identifier, parameters, statements, returns, public }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}func {}(", if self.public { "#" } else { "" }, self.identifier)?;
        for (i, param) in self.parameters.iter().enumerate() {
            write!(f, "{}", param)?;
            if i < self.parameters.len() - 1 {
                write!(f, ", ")?;
            }
        }
        writeln!(f, "): {} {{", self.returns)?;
        for stat in self.statements.iter() {
            writeln!(f, "{}", stat)?;
        }
        write!(f, "}}")
    }
}

pub type FunctionNode = Node<Function>;