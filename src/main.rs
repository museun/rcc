use std::env::args;

macro_rules! fail {
    ($($arg:tt)*) => {{
        eprintln!($($arg)*);
        std::process::exit(1)
    }};
}

fn main() {
    let args = args();
    if args.len() != 2 {
        fail!("usage: rcc <code>");
    }
    let args = args.skip(1).collect::<String>();
    let input: &str = args.as_ref();
    let tokens = tokenize(&input);

    if tokens.is_empty() {
        fail!("didn't tokenize anything");
    }

    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");

    // for now, starts with a number
    if let TokenType::Num(n) = tokens[0].ty {
        println!("  mov rax, {}", n);
    } else {
        fail!("requires a number to push onto the stack")
    }

    // this should honestly be recursive
    let mut iter = tokens.iter().skip(1).enumerate().peekable();
    while let Some((i, token)) = iter.next() {
        match token.ty {
            TokenType::Add => {
                if let Some((_, tok)) = iter.peek() {
                    if let TokenType::Num(n) = tok.ty {
                        println!("  add rax, {}", n);
                    } else {
                        fail!("{}> add requires a number", i);
                    }
                } else {
                    fail!("{}> add requires a number", i);
                }
            }
            TokenType::Sub => {
                if let Some((_, tok)) = iter.peek() {
                    if let TokenType::Num(n) = tok.ty {
                        println!("  sub rax, {}", n);
                    } else {
                        fail!("{}> sub requires a number", i);
                    }
                } else {
                    fail!("{}> sub requires a number", i);
                }
            }
            TokenType::EOF => {
                println!("\tret");
                // probably should do this elsewhere
            }
            _ => {}
        }
    }
}

#[derive(Debug, PartialEq)]
enum TokenType {
    Add,
    Sub,
    Num(u32),
    EOF,
}

#[derive(Debug)]
struct Token<'a> {
    ty: TokenType,
    s: &'a str,
}

impl<'a> Token<'a> {
    pub fn new(s: &'a str, ty: TokenType) -> Self {
        Self { ty, s }
    }
}

fn tokenize(s: &str) -> Vec<Token> {
    let mut tokens = vec![];
    let mut skip = 0;

    for (i, c) in s.chars().enumerate() {
        if skip > 0 {
            skip -= 1;

            continue;
        }

        if c.is_whitespace() {
            continue;
        }

        let token = match c {
            '+' => Token::new(&s[i..], TokenType::Add),
            '-' => Token::new(&s[i..], TokenType::Sub),
            c if c.is_ascii_digit() => {
                let k: u32 = s[i..]
                    .chars()
                    .take_while(|c| c.is_ascii_digit())
                    .filter_map(|c| c.to_digit(10))
                    .inspect(|_| skip += 1)
                    .fold(0, |a, n| 10 * a + n);
                skip -= 1; // this is off by 1 one because of the filter_map
                Token::new(&s[i + skip..], TokenType::Num(k))
            }
            _ => {
                fail!("cannot tokenize: {} @ {} '{}'", c, i, &s[i..]);
            }
        };

        tokens.push(token)
    }

    tokens.push(Token::new("", TokenType::EOF));
    tokens
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_tokenize() {
        use TokenType::*;
        let expected = vec![
            Num(10),
            Add,
            Num(5),
            Sub,
            Num(10),
            Add,
            Num(1),
            Sub,
            Num(5),
            Add,
            Num(5),
            Add,
            Num(10),
            EOF,
        ];

        let input = "10 + 5 - 10+1-5+5     +  10 ";
        let tokens = tokenize(&input);
        for (i, tok) in tokens.iter().enumerate() {
            assert_eq!(tok.ty, expected[i]);
        }
    }
}
