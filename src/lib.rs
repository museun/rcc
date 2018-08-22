#![feature(nll)]

pub(crate) const MAX_INST: usize = 1000;

#[macro_use]
mod util;
pub use util::*;

mod parse;
pub use parse::*;

mod token;
pub use token::*;

mod ir;
pub use ir::*;

mod reg;
pub use reg::*;

mod codegen;
pub use codegen::*;
