use std::env::args;

fn main() {
    let args = args();
    if args.len() != 2 {
        eprintln!("usage: rcc <code>");
        ::std::process::exit(1)
    }
    let mut args = args.skip(1);

    println!(".intel_syntax noprefix");

    println!(".global main");
    println!("main:");
    println!(
        "\tmov rax, {}",
        args.next()
            .expect("a number")
            .parse::<usize>()
            .expect("a number")
    );
    println!("\tret");
}
