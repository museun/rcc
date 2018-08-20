use node::{Node, NodeType};
use token::Token;

const REGISTERS: [&str; 8] = ["rdi", "rsi", "r10", "r11", "r12", "r13", "r14", "r15"];

pub fn generate<'a>(
    mut depth: &mut usize,
    tokens: &'a impl Iterator<Item = Token>,
    node: &Node,
) -> &'a str {
    if let NodeType::Num(n) = node.ty {
        if *depth + 1 > REGISTERS.len() {
            fail!("registers exhausted");
        }
        let reg = REGISTERS[*depth];
        *depth += 1;
        println!("  mov {}, {}", reg, n);
        return reg;
    }

    let dst = generate(&mut depth, tokens, node.lhs.as_ref().unwrap());
    let src = generate(&mut depth, tokens, node.rhs.as_ref().unwrap());

    match node.ty {
        NodeType::Add => {
            println!("  add {}, {}", dst, src);
            dst
        }
        NodeType::Sub => {
            println!("  sub {}, {}", dst, src);
            dst
        }
        ref e => panic!("unknown operator: {:?}", e), // panic so we can have a stack trace
    }
}
