use std::env::args;

#[macro_use]
extern crate rcc;
use rcc::*;

fn main() {
    let args = args();
    if args.len() == 1 {
        return;
    }

    let mut args = args.skip(1);
    match &args.next().unwrap()[..] {
        "test" => {
            let input = args.next().unwrap().parse::<usize>().expect("a number");
            let (_, input) = test::TESTS[input];
            compile(&input);
        }
        input => compile(&input),
    }
}

fn compile(input: &str) {
    eprintln!("{} {}", wrap_color!(Color::Yellow {}, "input:"), input);

    let tokens = Lexer::tokenize(&input);
    eprintln!("{}", wrap_color!(Color::Yellow {}, "tokens:"));
    eprintln!("{:#?}", tokens);

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

    eprintln!("{}", wrap_color!(Color::Yellow {}, "asm:"));
    generate_x64(&ABI::SystemV, ir);
}
