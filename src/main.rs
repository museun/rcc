use std::env::args;

fn main() {
    let args = args();
    if args.len() != 2 {
        eprintln!("usage: rcc <code>");
        ::std::process::exit(1)
    }
    let args = args.skip(1).collect::<String>();
    let mut input: &str = args.as_ref();

    fn parse_next_number(input: &mut &str) -> usize {
        let mut pos = 0;
        let n = input
            .chars()
            .take_while(|c| c.is_digit(10))
            .inspect(|_| pos += 1)
            .collect::<String>()
            .parse::<usize>()
            .expect("a number");
        *input = &input[pos..];
        n
    }

    fn add(input: &mut &str) {
        let n = parse_next_number(input);
        println!("  add rax, {}", n);
    }

    fn sub(input: &mut &str) {
        let n = parse_next_number(input);
        println!("  sub rax, {}", n);
    }

    pub fn advance(input: &mut &str) { *input = &input[1..]; }

    let data = parse_next_number(&mut input);

    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");
    println!("  mov rax, {}", data);

    while !input.is_empty() {
        if input.get(0..1) == Some("+") {
            advance(&mut input);
            add(&mut input);
            continue;
        }
        if input.get(0..1) == Some("-") {
            advance(&mut input);
            sub(&mut input);
            continue;
        }

        eprintln!("expected character: {:?}", &args[0..1]);
    }

    println!("\tret");
}
