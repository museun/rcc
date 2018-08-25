#![feature(nll)]
#![recursion_limit = "1024"]

pub(crate) const MAX_INST: usize = 1000;

#[macro_use]
mod util;
pub use util::*;

mod parser;
pub use parser::*;

mod lexer;
pub use lexer::*;

mod ir;
pub use ir::*;

mod registers;
pub use registers::*;

mod codegen;
pub use codegen::*;

mod semantics;
pub use semantics::*;

mod print;
pub use print::*;

pub mod test;
