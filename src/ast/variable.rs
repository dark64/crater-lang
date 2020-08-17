use crate::ast::node::Node;
use crate::ast::types::Type;
use crate::ast::Identifier;
use core::fmt;

#[derive(Debug, Clone)]
pub struct Variable<'ast> {
    pub id: Identifier<'ast>,
    pub type_: Type,
}

pub type VariableNode<'ast> = Node<Variable<'ast>>;

impl<'ast> fmt::Display for Variable<'ast> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.id, self.type_)
    }
}
