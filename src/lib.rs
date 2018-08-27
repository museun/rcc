#![feature(nll)]
#![recursion_limit = "1024"]

pub(crate) const MAX_INST: usize = 1000;

#[macro_use]
mod util;
pub use util::*;

mod lexer;
pub use lexer::*;

mod types;
pub use types::*;

mod kind;
pub use kind::*;

mod parser;
pub use parser::*;

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

pub mod frontend;

pub mod test;
