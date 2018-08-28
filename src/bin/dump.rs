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
        compile("stdin", &buffer);
        return;
    }

    let mut args = args.skip(1);
    match &args.next().unwrap()[..] {
        "test" => {
            let no = args.next().unwrap().parse::<usize>().expect("a number");
            let input = get_source_for(no).expect("to read a file");
            compile(&format!("tests/test_{:04}.c", no), &input);
        }
        input => compile("stdin", &input),
    }
}

fn get_source_for(n: usize) -> Option<String> {
    fs::read_to_string(&format!("tests/test_{:04}.c", n)).ok()
}

fn compile(file: &str, input: &str) {
    eprintln!("{} {}", wrap_color!(Color::Yellow {}, "input:"), input);

    let tokens = Tokens::tokenize(file, &input);
    eprintln!("{}", wrap_color!(Color::Yellow {}, "tokens:"));
    eprintln!("{}", tokens);

    let mut ast = Parser::parse(tokens);
    eprintln!("{}", wrap_color!(Color::Yellow {}, "ast:"));
    eprintln!("{:#?}", ast);

    let nodes = Semantics::analyze(&mut ast);
    eprintln!("{}", wrap_color!(Color::Yellow {}, "semantics:"));
    eprintln!("{:#?}", nodes);

    let mut ir = Generate::gen_ir(&nodes);
    eprintln!("{}", wrap_color!(Color::Yellow {}, "ir:"));
    for func in &ir {
        eprintln!("{}(): -- {}", func.name, func.stacksize);
        if !func.globals.is_empty() {
            eprintln!("  globals:");
            for g in &func.globals {
                eprintln!("    {:?}", g);
            }
            eprintln!();
        }
        for ir in &func.ir {
            eprintln!("  {}", ir);
        }
    }

    let ir = Registers::allocate(&mut ir);
    eprintln!("{}", wrap_color!(Color::Yellow {}, "reg:"));
    for func in ir.iter() {
        eprintln!("{}(): -- {}", func.name, func.stacksize);
        if !func.globals.is_empty() {
            eprintln!("  globals:");
            for g in &func.globals {
                eprintln!("    {:?}", g);
            }
            eprintln!();
        }
        for ir in &func.ir {
            eprintln!("  {}", ir);
        }
    }

    let asm = generate_x64(&ABI::SystemV, ir);
    eprintln!("{}", wrap_color!(Color::Yellow {}, "asm:"));
    eprintln!("{}", asm);
}
