#![feature(nll)]
#![recursion_limit = "1024"]

// TODO remove this
pub(crate) const MAX_INST: usize = 1000;

#[macro_use]
mod util;
pub use util::*;

mod span; // code spans for reporting
pub use span::*;

mod types;
pub use types::*;

mod tokens;
pub use tokens::{Type as TokType, *};

mod lexer;
pub use lexer::*;

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

pub mod frontend;

pub mod test;
