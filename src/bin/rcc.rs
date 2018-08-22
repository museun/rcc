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

    let mut tokens = Tokens::tokenize(&input);
    if tokens.is_empty() {
        fail!("didn't tokenize anything");
    }
    let nodes = Node::parse(&mut tokens);
    let mut ir = Generate::gen_ir(&nodes);
    Registers::allocate(&mut ir);
    generate_x86(ir);
}
