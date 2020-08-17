use crate::ast::node::Node;
use crate::ast::types::{Type, TypeNode};
use crate::ast::variable::{Variable, VariableNode};
use core::fmt;

mod node;
mod types;
mod variable;

pub type Identifier<'ast> = &'ast str;
pub type FunctionIdentifier<'ast> = Identifier<'ast>;

pub enum Expression<'ast> {
    Identifier(Identifier<'ast>),
    Int8Constant(i8),
    Int16Constant(i16),
    Int32Constant(i32),
    Int64Constant(i64),
    UInt8Constant(u8),
    UInt16Constant(u16),
    UInt32Constant(u32),
    UInt64Constant(u64),
    BooleanConstant(bool),
    Add(Box<Expression<'ast>>, Box<Expression<'ast>>),
    Subtract(Box<Expression<'ast>>, Box<Expression<'ast>>),
    Multiply(Box<Expression<'ast>>, Box<Expression<'ast>>),
    Divide(Box<Expression<'ast>>, Box<Expression<'ast>>),
    FunctionCall(FunctionIdentifier<'ast>, Vec<ExpressionNode<'ast>>),
}

pub type ExpressionNode<'ast> = Node<Expression<'ast>>;

impl<'ast> fmt::Display for Expression<'ast> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Expression::Int8Constant(ref i) => write!(f, "{}", i),
            Expression::Int16Constant(ref i) => write!(f, "{}", i),
            Expression::Int32Constant(ref i) => write!(f, "{}", i),
            Expression::Int64Constant(ref i) => write!(f, "{}", i),
            Expression::UInt8Constant(ref i) => write!(f, "{}", i),
            Expression::UInt16Constant(ref i) => write!(f, "{}", i),
            Expression::UInt32Constant(ref i) => write!(f, "{}", i),
            Expression::UInt64Constant(ref i) => write!(f, "{}", i),
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
pub enum Assignee<'ast> {
    Identifier(Variable<'ast>),
}

pub type AssigneeNode<'ast> = Node<Assignee<'ast>>;

impl<'ast> fmt::Display for Assignee<'ast> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Assignee::Identifier(ref var) => write!(f, "{}", var),
        }
    }
}

pub enum Statement<'ast> {
    Return(ExpressionNode<'ast>),
    Declaration(VariableNode<'ast>),
    Definition(AssigneeNode<'ast>, ExpressionNode<'ast>),
}

pub type StatementNode<'ast> = Node<Statement<'ast>>;

impl<'ast> fmt::Display for Statement<'ast> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Statement::Return(ref expr) => write!(f, "return {}", expr),
            Statement::Declaration(ref var) => write!(f, "{}", var),
            Statement::Definition(ref assignee, ref expr) => write!(f, "{} = {}", assignee, expr),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub inputs: Vec<TypeNode>,
    pub output: TypeNode,
}

type ParameterNode<'ast> = VariableNode<'ast>;

pub struct Function<'ast> {
    pub arguments: Vec<ParameterNode<'ast>>,
    pub statements: Vec<StatementNode<'ast>>,
    pub signature: FunctionSignature,
}

pub type FunctionNode<'ast> = Node<Function<'ast>>;
