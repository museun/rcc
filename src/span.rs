use util::*;

use std::fmt;

#[derive(Debug, Clone, Copy)]
pub struct Span<'a> {
    file: &'a str,
    line: usize,
    col: usize,
}

impl<'a> Span<'a> {
    pub fn new(file: &'a str, line: usize, col: usize) -> Self {
        Self { file, line, col }
    }

    pub fn total_width(&self) -> usize {
        // 2 for the colons
        2 + self.file.len() + count_digits(self.line) + count_digits(self.col)
    }
}

impl<'a> fmt::Display for Span<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.col)
    }
}
