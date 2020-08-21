use crate::ast::node::Node;
use core::fmt;
use std::convert::TryFrom;

pub type TypeError = String;

#[derive(Debug, Clone)]
pub enum Type {
    Int(usize),
    UInt(usize),
    Boolean
}

pub type TypeNode = Node<Type>;

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Int(bit_width) => write!(f, "int{}", bit_width),
            Type::UInt(bit_width) => write!(f, "uint{}", bit_width),
            Type::Boolean => write!(f, "bool")
        }
    }
}

impl TryFrom<String> for Type {
    type Error = TypeError;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        match value.as_str() {
            "int32" => Ok(Type::Int(32)),
            "uint32" => Ok(Type::UInt(32)),
            "bool" => Ok(Type::Boolean),
            _ => Err(TypeError::from(format!("Unknown type {}", value)))
        }
    }
}