use crate::ast::node::Node;
use core::fmt;

#[derive(Debug, Clone)]
pub enum Type {
    Int(usize),
    UInt(usize),
    Boolean,
    Array(Box<Type>, usize),
}

pub type TypeNode = Node<Type>;

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Int(bit_width) => write!(f, "int{}", bit_width),
            Type::UInt(bit_width) => write!(f, "uint{}", bit_width),
            Type::Boolean => write!(f, "bool"),
            Type::Array(ref ty, ref size) => write!(f, "{}[{}]", ty, size),
        }
    }
}
