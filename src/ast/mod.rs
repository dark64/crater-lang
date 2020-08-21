use crate::ast::node::Node;
use crate::ast::types::{TypeNode, Type};
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
    Sub(Box<ExpressionNode>, Box<ExpressionNode>),
    Mul(Box<ExpressionNode>, Box<ExpressionNode>),
    Div(Box<ExpressionNode>, Box<ExpressionNode>),
    Pow(Box<ExpressionNode>, Box<ExpressionNode>),
    Mod(Box<ExpressionNode>, Box<ExpressionNode>),
    Eq(Box<ExpressionNode>, Box<ExpressionNode>),
    Neq(Box<ExpressionNode>, Box<ExpressionNode>),
    Lt(Box<ExpressionNode>, Box<ExpressionNode>),
    Le(Box<ExpressionNode>, Box<ExpressionNode>),
    Ge(Box<ExpressionNode>, Box<ExpressionNode>),
    Gt(Box<ExpressionNode>, Box<ExpressionNode>),
    And(Box<ExpressionNode>, Box<ExpressionNode>),
    Or(Box<ExpressionNode>, Box<ExpressionNode>),
    Not(Box<ExpressionNode>),
    Ternary(
        Box<ExpressionNode>,
        Box<ExpressionNode>,
        Box<ExpressionNode>,
    ),
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
            Expression::Sub(ref left, ref right) => write!(f, "({} - {})", left, right),
            Expression::Mul(ref left, ref right) => write!(f, "({} * {})", left, right),
            Expression::Div(ref left, ref right) => write!(f, "({} / {})", left, right),
            Expression::Pow(ref left, ref right) => write!(f, "({} ** {})", left, right),
            Expression::Mod(ref left, ref right) => write!(f, "({} % {})", left, right),
            Expression::Eq(ref left, ref right) => write!(f, "({} == {})", left, right),
            Expression::Neq(ref left, ref right) => write!(f, "({} != {})", left, right),
            Expression::Lt(ref left, ref right) => write!(f, "({} < {})", left, right),
            Expression::Le(ref left, ref right) => write!(f, "({} <= {})", left, right),
            Expression::Ge(ref left, ref right) => write!(f, "({} >= {})", left, right),
            Expression::Gt(ref left, ref right) => write!(f, "({} > {})", left, right),
            Expression::And(ref left, ref right) => write!(f, "({} && {})", left, right),
            Expression::Or(ref left, ref right) => write!(f, "({} || {})", left, right),
            Expression::Not(ref expr) => write!(f, "!{}", expr),
            Expression::Ternary(ref condition, ref consequent, ref alternative) => {
                write!(f, "{} ? {} : {}", condition, consequent, alternative)
            }
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
    Return(Box<ExpressionNode>),
    Declaration(Box<VariableNode>),
    Definition(Box<VariableNode>, Box<ExpressionNode>),
    Assignment(Box<AssigneeNode>, Box<ExpressionNode>),
}

pub type StatementNode = Node<Statement>;

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Statement::Return(ref expr) => write!(f, "\treturn {};", expr),
            Statement::Declaration(ref var) => write!(f, "\tlet {};", var),
            Statement::Definition(ref var, ref expr) => {
                write!(f, "\tlet {} = {};", var, expr)
            }
            Statement::Assignment(ref assignee, ref expr) => {
                write!(f, "\t{} = {};", assignee, expr)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub inputs: Vec<Type>,
    pub output: Type,
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
    pub fn new(
        identifier: Identifier,
        parameters: Vec<ParameterNode>,
        statements: Vec<StatementNode>,
        returns: TypeNode,
        public: bool,
    ) -> Self {
        Self {
            identifier,
            parameters,
            statements,
            returns,
            public,
        }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}func {}(",
            if self.public { "#" } else { "" },
            self.identifier
        )?;
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
