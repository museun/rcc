use std::env::args;
use std::fs;

#[macro_use]
extern crate rcc;
use rcc::*;

fn main() {
    let args = args();
    let input = if args.len() != 2 {
        fail!("usage: rcc file");
    } else {
        args.skip(1).collect::<String>()
    };

    let fi = fs::read_to_string(&input).expect("to read file");
    match frontend::compile(&input, fi) {
        Ok(output) => println!("{}", output),
        Err(_err) => eprintln!("error!"),
    }
}
