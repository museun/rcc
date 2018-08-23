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
            "run" => {
                let args = args.skip(2).collect::<String>();
                let input = args.parse::<usize>().expect("a number");

                let (_, input) = test::TESTS[input];

                eprintln!("input: {}", input);

                let mut tokens = Tokens::tokenize(&input);
                eprintln!("tokens:\n{:#?}", tokens);

                let mut ast = Node::parse(&mut tokens);
                eprintln!("ast:\n{:#?}", ast);
                let mut nodes = ast.iter_mut().collect::<Vec<_>>();

                let nodes = Semantics::analyze(&mut nodes);
                eprintln!("semantics:\n{:#?}", nodes);

                let mut ir = Generate::gen_ir(&nodes);
                eprintln!("ir:\n{:#?}", ir);

                Registers::allocate(&mut ir);
                eprintln!("reg:\n{:#?}", ir);

                generate_x64(ABI::SystemV, ir);
            }
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
                generate_x64(ABI::SystemV, ir);
            }
            "asm_win" => {
                let args = args.skip(2).collect::<String>();
                let input: &str = args.as_ref();
                let mut tokens = Tokens::tokenize(&input);
                let ast = Node::parse(&mut tokens);
                let mut ir = Generate::gen_ir(&ast);
                Registers::allocate(&mut ir);
                generate_x64(ABI::Windows, ir);
            }
            _ => {}
        }
    }
}
