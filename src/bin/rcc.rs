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

    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");

    // for now, starts with a number
    if let TokenType::Num(n) = tokens[0].ty {
        println!("  mov rax, {}", n);
    } else {
        fail!("requires a number to push onto the stack")
    }

    // this should honestly be recursive
    let mut iter = tokens.iter().skip(1).enumerate().peekable();
    while let Some((i, token)) = iter.next() {
        match token.ty {
            TokenType::Add => {
                if let Some((_, tok)) = iter.peek() {
                    if let TokenType::Num(n) = tok.ty {
                        println!("  add rax, {}", n);
                    } else {
                        fail!("{}> add requires a number", i);
                    }
                } else {
                    fail!("{}> add requires a number", i);
                }
            }
            TokenType::Sub => {
                if let Some((_, tok)) = iter.peek() {
                    if let TokenType::Num(n) = tok.ty {
                        println!("  sub rax, {}", n);
                    } else {
                        fail!("{}> sub requires a number", i);
                    }
                } else {
                    fail!("{}> sub requires a number", i);
                }
            }
            TokenType::EOF => {
                println!("\tret");
                // probably should do this elsewhere
            }
            _ => {}
        }
    }
}
