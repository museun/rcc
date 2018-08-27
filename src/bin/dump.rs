use std::{
    env::args,
    fs,
    io::{self, prelude::*},
};

#[macro_use]
extern crate rcc;
use rcc::*;

fn main() {
    let args = args();
    if args.len() == 1 {
        let mut buffer = String::new();
        io::stdin()
            .read_to_string(&mut buffer)
            .expect("to read stdin");
        compile(&buffer);
        return;
    }

    let mut args = args.skip(1);
    match &args.next().unwrap()[..] {
        "test" => {
            let input = args.next().unwrap().parse::<usize>().expect("a number");
            let input = get_source_for(input).expect("to read a file");
            compile(&input);
        }
        input => compile(&input),
    }
}

fn get_source_for(n: usize) -> Option<String> {
    fs::read_to_string(&format!("tests/test_{:04}.c", n)).ok()
}

fn compile(input: &str) {
    eprintln!("{} {}", wrap_color!(Color::Yellow {}, "input:"), input);

    let tokens = Tokens::tokenize(&input);
    eprintln!("{}", wrap_color!(Color::Yellow {}, "tokens:"));
    eprintln!("{}", tokens);

    let mut ast = Node::parse(tokens);
    eprintln!("{}", wrap_color!(Color::Yellow {}, "ast:"));
    print_ast(&ast);

    let nodes = Semantics::analyze(&mut ast);
    eprintln!("{}", wrap_color!(Color::Yellow {}, "semantics:"));
    print_ast(&nodes);

    let mut ir = Generate::gen_ir(&nodes);
    eprintln!("{}", wrap_color!(Color::Yellow {}, "ir:"));
    eprintln!("{:#?}", ir);

    let ir = Registers::allocate(&mut ir);
    eprintln!("{}", wrap_color!(Color::Yellow {}, "reg:"));
    eprintln!("{:#?}", ir);

    let asm = generate_x64(&ABI::SystemV, ir);
    eprintln!("{}", wrap_color!(Color::Yellow {}, "asm:"));
    eprintln!("{}", asm);
}
