#![feature(nll)]

pub(crate) const MAX_INST: usize = 1000;
pub(crate) const REGS: [&str; 8] = ["rdi", "rsi", "r10", "r11", "r12", "r13", "r14", "r15"];

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
