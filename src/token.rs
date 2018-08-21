use std::ops::{Index, IndexMut};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Add,
    Sub,
    Mul,
    Div,
    Num(u32),
    EOF,
}

#[derive(Debug)]
pub struct Tokens {
    data: Vec<Token>,
    pos: usize,
}

impl Tokens {
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn next_token(&mut self) -> Option<&Token> {
        let tok = self.data.get(self.pos);
        if tok.is_some() {
            self.pos += 1;
        }
        tok
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    pub fn advance(&mut self) {
        self.pos += 1;
    }

    pub fn peek(&mut self) -> Option<&Token> {
        self.data.get(self.pos)
    }

    pub fn tokenize(s: &str) -> Self {
        let mut data = vec![];
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
                '+' => Token::Add,
                '-' => Token::Sub,
                '*' => Token::Mul,
                '/' => Token::Div,
                c if c.is_ascii_digit() => {
                    let k: u32 = s[i..]
                        .chars()
                        .take_while(|c| c.is_ascii_digit())
                        .filter_map(|c| c.to_digit(10))
                        .inspect(|_| skip += 1)
                        .fold(0, |a, n| 10 * a + n);
                    skip -= 1; // this is off by 1 one because of the filter_map
                    Token::Num(k)
                }
                _ => {
                    fail!("cannot tokenize: {} @ {} '{}'", c, i, &s[i..]);
                }
            };

            data.push(token)
        }

        data.push(Token::EOF);
        Self { data, pos: 0 }
    }
}

impl Index<usize> for Tokens {
    type Output = Token;

    fn index(&self, p: usize) -> &Self::Output {
        &self.data[p]
    }
}

impl IndexMut<usize> for Tokens {
    fn index_mut(&mut self, p: usize) -> &mut Token {
        &mut self.data[p]
    }
}
