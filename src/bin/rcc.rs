use std::env::args;

#[macro_use]
extern crate rcc;
use rcc::*;

fn main() {
    let args = args();
    if args.len() != 2 {
        fail!("usage: rcc <code>");
    }
    let args = args.skip(1).collect::<String>();
    let input: &str = args.as_ref();

    let mut tokens = Lexer::tokenize(&input);
    if tokens.is_empty() {
        fail!("didn't tokenize anything");
    }
    let mut nodes = Node::parse(&mut tokens);
    let mut nodes = nodes.iter_mut().collect::<Vec<_>>();
    let nodes = Semantics::analyze(&mut nodes);
    let mut ir = Generate::gen_ir(&nodes);
    Registers::allocate(&mut ir); // TODO: this should return the new IR
    generate_x64(&ABI::SystemV, ir);
}
