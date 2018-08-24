use std::env::args;

#[macro_use]
extern crate rcc;
use rcc::*;

fn main() {
    let args = args();
    if args.len() != 3 {
        fail!("usage: dump <option1> <option2>");
    }

    if let Some(arg) = ::std::env::args().nth(1) {
        match &arg[..] {
            // TODO fix this
            // #[cfg(any(test, feature = "dump"))]
            // "run" => {
            //     let args = args.skip(2).collect::<String>();
            //     let input = args.parse::<usize>().expect("a number");

            //     let (_, input) = test::TESTS[input];

            //     eprintln!("{}: {}", wrap_color!(Color::Yellow {}, "input"), input);

            //     let mut tokens = Tokens::tokenize(&input);
            //     eprintln!("{}", wrap_color!(Color::Yellow {}, "tokens:"));
            //     eprintln!("{:#?}", tokens);

            //     let mut ast = Node::parse(&mut tokens);
            //     eprintln!("{}", wrap_color!(Color::Yellow {}, "ast:"));
            //     dump_ast(&ast);

            //     let mut nodes = ast.iter_mut().collect::<Vec<_>>();
            //     let nodes = Semantics::analyze(&mut nodes);
            //     eprintln!("{}", wrap_color!(Color::Yellow {}, "semantics:"));
            //     dump_ast(&nodes);

            //     let mut ir = Generate::gen_ir(&nodes);
            //     eprintln!("{}", wrap_color!(Color::Yellow {}, "ir:"));
            //     eprintln!("{:#?}", ir);

            //     Registers::allocate(&mut ir);
            //     eprintln!("{}", wrap_color!(Color::Yellow {}, "reg:"));
            //     eprintln!("{:#?}", ir);

            //     eprintln!("{}", wrap_color!(Color::Yellow {}, "asm:"));
            //     generate_x64(&ABI::SystemV, ir);
            // }
            // naming is hard
            "tok" => {
                let args = args.skip(2).collect::<String>();
                let input: &str = args.as_ref();
                eprintln!("{:#?}", Tokens::tokenize(&input));
            }
            "ast" => {
                let args = args.skip(2).collect::<String>();
                let input: &str = args.as_ref();
                let mut tokens = Tokens::tokenize(&input);
                let ast = Node::parse(&mut tokens);
                print_ast(&ast);
                // eprintln!("{}", &input);
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
                generate_x64(&ABI::SystemV, ir);
            }
            "all" => {
                let args = args.skip(2).collect::<String>();
                let input: &str = args.as_ref();
                eprintln!("{}: {}", wrap_color!(Color::Yellow {}, "input"), input);

                let mut tokens = Tokens::tokenize(&input);
                eprintln!("{}", wrap_color!(Color::Yellow {}, "tokens:"));
                eprintln!("{:#?}", tokens);

                let mut ast = Node::parse(&mut tokens);
                eprintln!("{}", wrap_color!(Color::Yellow {}, "ast:"));
                print_ast(&ast);

                let mut nodes = ast.iter_mut().collect::<Vec<_>>();
                let nodes = Semantics::analyze(&mut nodes);
                eprintln!("{}", wrap_color!(Color::Yellow {}, "semantics:"));
                print_ast(&nodes);

                let mut ir = Generate::gen_ir(&nodes);
                eprintln!("{}", wrap_color!(Color::Yellow {}, "ir:"));
                eprintln!("{:#?}", ir);

                Registers::allocate(&mut ir);
                eprintln!("{}", wrap_color!(Color::Yellow {}, "reg:"));
                eprintln!("{:#?}", ir);

                eprintln!("{}", wrap_color!(Color::Yellow {}, "asm:"));
                generate_x64(&ABI::SystemV, ir);
            }
            _ => {}
        }
    }
}
