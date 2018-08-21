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

    let tokens = tokenize(&input);
    if tokens.is_empty() {
        fail!("didn't tokenize anything");
    }

    let mut iter = &mut tokens.into_iter();
    let root = Node::expr(&mut iter);

    let mut gen = Generate::new();
    gen.gen_ir(&root);
    gen.allocate_registers();

    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");

    gen.generate();
}
