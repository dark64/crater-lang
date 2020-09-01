use core::fmt;
use crate::common::Position;

#[derive(Debug, Clone)]
pub struct Node<T> {
    pub start: Position,
    pub value: T,
}

impl<T: fmt::Display> Node<T> {
    pub fn from(value: T, start: Position) -> Self {
        Self { value, start }
    }
}

impl<T: fmt::Display> fmt::Display for Node<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
