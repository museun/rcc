use std::{
    env::args,
    io::{self, prelude::*},
};

#[macro_use]
extern crate rcc;
use rcc::*;

fn main() {
    let args = args();
    let input = if args.len() == 1 {
        let mut buffer = String::new();
        io::stdin()
            .read_to_string(&mut buffer)
            .expect("to read stdin");
        buffer
    } else if args.len() != 2 {
        fail!("usage: rcc <code>");
    } else {
        args.skip(1).collect::<String>()
    };

    let tokens = Tokens::tokenize("stdin", &input);
    if tokens.is_empty() {
        fail!("didn't tokenize anything");
    }
    let mut ast = Node::parse(tokens);
    let ast = Semantics::analyze(&mut ast);
    let mut ir = Generate::gen_ir(&ast);
    let ir = Registers::allocate(&mut ir);
    println!("{}", generate_x64(&ABI::SystemV, ir));
}
