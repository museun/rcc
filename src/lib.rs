#![allow(dead_code)]
#![feature(nll)]

#[macro_use]
mod util;
pub use util::*;

use std::cell::RefCell;
use std::rc::Rc;
type RefType = Rc<RefCell<types::Type>>;

pub mod codegen;
pub mod frontend;
pub mod ir;
pub mod kind;
pub mod lexer;
pub mod node;
pub mod parser;
pub mod registers;
pub mod semantics;
pub mod span;
pub mod tokens;
pub mod types;

pub mod test;
