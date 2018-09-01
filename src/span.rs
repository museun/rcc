use std::fmt;
use util::*;

#[derive(Debug, Clone)]
pub struct Span {
    file: String,
    line: usize,
    col: usize,
}

impl Span {
    pub fn new(file: &str, line: usize, col: usize) -> Self {
        Self {
            file: file.into(),
            line,
            col,
        }
    }

    pub fn total_width(&self) -> usize {
        // 2 for the colons
        2 + self.file.len() + count_digits(self.line) + count_digits(self.col)
    }

    pub fn row(&self) -> usize {
        self.line
    }

    pub fn column(&self) -> usize {
        self.col
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.col)
    }
}
