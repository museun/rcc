#[derive(Debug, PartialEq)]
pub enum TokenType {
    Add,
    Sub,
    Num(u32),
    EOF,
}

#[derive(Debug)]
pub struct Token<'a> {
    pub ty: TokenType,
    pub s: &'a str,
}

impl<'a> Token<'a> {
    pub fn new(s: &'a str, ty: TokenType) -> Self {
        Self { ty, s }
    }
}

pub fn tokenize(s: &str) -> Vec<Token> {
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
