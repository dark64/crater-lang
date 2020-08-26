use crate::ast::node::Node;
use crate::ast::types::{Type, TypeNode};
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
    StringConstant(String),
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
            Expression::BooleanConstant(ref b) => write!(f, "{}", b),
            Expression::StringConstant(ref s) => write!(f, "{}", s),
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
            Expression::FunctionCall(ref id, ref expressions) => {
                write!(f, "{}(", id)?;
                for (i, param) in expressions.iter().enumerate() {
                    write!(f, "{}", param)?;
                    if i < expressions.len() - 1 {
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
    Condition(
        Box<ExpressionNode>,
        Box<Vec<StatementNode>>,
        Option<Box<Vec<StatementNode>>>,
    ),
    FunctionCall(FunctionIdentifier, Vec<ExpressionNode>),
}

pub type StatementNode = Node<Statement>;

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Statement::Return(ref expr) => writeln!(f, "return {};", expr),
            Statement::Declaration(ref var) => writeln!(f, "let {};", var),
            Statement::Definition(ref var, ref expr) => writeln!(f, "let {} = {};", var, expr),
            Statement::Assignment(ref assignee, ref expr) => {
                writeln!(f, "{} = {};", assignee, expr)
            }
            Statement::Condition(..) => self.fmt_indented(f, 1),
            Statement::FunctionCall(ref id, ref expressions) => {
                write!(f, "{}(", id)?;
                for (i, param) in expressions.iter().enumerate() {
                    write!(f, "{}", param)?;
                    if i < expressions.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                writeln!(f, ");")
            }
        }
    }
}

impl Statement {
    pub fn fmt_indented(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
        match self {
            Statement::Condition(ref expr, ref consequent, ref alternative) => {
                writeln!(f, "{}if {} {{", "\t".repeat(depth), expr)?;
                for stat in consequent.iter() {
                    stat.value.fmt_indented(f, depth + 1)?;
                }
                if let Some(alternative) = alternative {
                    writeln!(f, "{} }} else {{", "\t".repeat(depth))?;
                    for stat in alternative.iter() {
                        stat.value.fmt_indented(f, depth + 1)?;
                    }
                }
                writeln!(f, "{}}}", "\t".repeat(depth))
            }
            s => write!(f, "{}{}", "\t".repeat(depth), s),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub inputs: Vec<Type>,
    pub output: Option<Type>,
}

pub type ParameterNode = VariableNode;

#[derive(Debug, Clone)]
pub struct Function {
    pub identifier: Identifier,
    pub parameters: Vec<ParameterNode>,
    pub statements: Vec<StatementNode>,
    pub returns: Option<TypeNode>,
    pub signature: FunctionSignature,
    pub public: bool,
}

impl Function {
    pub fn new(
        identifier: Identifier,
        parameters: Vec<ParameterNode>,
        statements: Vec<StatementNode>,
        returns: Option<TypeNode>,
        signature: FunctionSignature,
        public: bool,
    ) -> Self {
        Self {
            identifier,
            parameters,
            statements,
            returns,
            signature,
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
        write!(f, ")")?;
        if let Some(ty) = &self.returns {
            write!(f, ": {}", ty)?;
        }
        writeln!(f, " {{")?;
        for stat in self.statements.iter() {
            stat.value.fmt_indented(f, 1)?
        }
        write!(f, "}}")
    }
}

pub type FunctionNode = Node<Function>;
