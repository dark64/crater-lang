use core::fmt;

#[derive(Debug, Clone, Default, Copy)]
pub struct Position {
    pub line: usize,
    pub col: usize,
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}
