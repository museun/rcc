use std::env::args;

#[macro_use]
extern crate rcc;
use rcc::*;

fn main() {
    let args = args();
    if args.len() != 3 {
        fail!("usage: dump <option> <code>");
    }

    if let Some(arg) = ::std::env::args().nth(1) {
        match &arg[..] {
            // naming is hard
            "tok" => {
                let args = args.skip(2).collect::<String>();
                let input: &str = args.as_ref();
                eprintln!("{:#?}", Tokens::tokenize(&input));
            }
            "tok_p" => {
                let args = args.skip(2).collect::<String>();
                let input: &str = args.as_ref();
                eprintln!("{}", Tokens::tokenize(&input));
            }
            "ast" => {
                let args = args.skip(2).collect::<String>();
                let input: &str = args.as_ref();
                let mut tokens = Tokens::tokenize(&input);
                eprintln!("{:#?}", Node::parse(&mut tokens));
            }
            "ast_c" => {
                let args = args.skip(2).collect::<String>();
                let input: &str = args.as_ref();
                let mut tokens = Tokens::tokenize(&input);
                eprintln!("{:?}", Node::parse(&mut tokens));
            }
            "ir" => {
                let args = args.skip(2).collect::<String>();
                let input: &str = args.as_ref();
                let mut tokens = Tokens::tokenize(&input);
                let ast = Node::parse(&mut tokens);
                eprintln!("{:#?}", Generate::gen_ir(&ast));
            }
            "reg" => {
                let args = args.skip(2).collect::<String>();
                let input: &str = args.as_ref();
                let mut tokens = Tokens::tokenize(&input);
                let ast = Node::parse(&mut tokens);
                let mut ir = Generate::gen_ir(&ast);
                Registers::allocate(&mut ir);
                eprintln!("{:#?}", ir);
            }
            "asm" => {
                let args = args.skip(2).collect::<String>();
                let input: &str = args.as_ref();
                let mut tokens = Tokens::tokenize(&input);
                let ast = Node::parse(&mut tokens);
                let mut ir = Generate::gen_ir(&ast);
                Registers::allocate(&mut ir);
                generate_x86(ir);
            }
            _ => {}
        }
    }
}
