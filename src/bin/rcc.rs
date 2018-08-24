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

    let tokens = Lexer::tokenize(&input);
    if tokens.is_empty() {
        fail!("didn't tokenize anything");
    }
    let mut ast = Node::parse(tokens);
    let ast = Semantics::analyze(&mut ast);
    let mut ir = Generate::gen_ir(&ast);
    let ir = Registers::allocate(&mut ir);
    generate_x64(&ABI::SystemV, ir);
}
