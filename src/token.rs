use std::{
    collections::HashMap,
    ops::{Index, IndexMut},
};

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, Token> = {
        let mut map = HashMap::new();
        map.insert("return", Token::Ret);
        map
    };
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Add,
    Sub,
    Mul,
    Div,
    Ret,
    Eos, // end of statement (expression? what are they called in legallese C)
    Num(u32),
    EOF,
}

#[derive(Debug, Clone)]
pub struct Tokens<'a> {
    data: Vec<(usize, Token)>,
    input: &'a str,
    pos: usize,
}

impl<'a> Tokens<'a> {
    pub fn tokenize(input: &'a str) -> Self {
        Self {
            data: scan(&input),
            input,
            pos: 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    pub fn next_token(&mut self) -> Option<&(usize, Token)> {
        let tok = self.data.get(self.pos);
        if tok.is_some() {
            self.pos += 1;
        }
        tok
    }

    pub fn peek(&mut self) -> Option<&(usize, Token)> {
        self.data.get(self.pos)
    }

    pub fn advance(&mut self) {
        self.pos += 1;
    }

    pub fn input_at(&self, pos: usize) -> &'a str {
        &self.input[pos..]
    }
}

impl<'a> Index<usize> for Tokens<'a> {
    type Output = Token;

    fn index(&self, p: usize) -> &Self::Output {
        &self.data[p].1
    }
}

impl<'a> IndexMut<usize> for Tokens<'a> {
    fn index_mut(&mut self, p: usize) -> &mut Token {
        &mut self.data[p].1
    }
}

fn scan(s: &str) -> Vec<(usize, Token)> {
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
            ';' => Token::Eos,

            // digits
            c if c.is_ascii_digit() => {
                let k: u32 = s[i..]
                    .chars()
                    .take_while(char::is_ascii_digit)
                    .filter_map(|c| c.to_digit(10))
                    .inspect(|_| skip += 1)
                    .fold(0, |a, n| 10 * a + n);
                skip -= 1; // this is off by 1 because of the filter_map
                Token::Num(k)
            }

            // keywords
            c if c.is_alphabetic() => {
                let name = s[i..]
                    .chars()
                    .take_while(|c| c.is_alphanumeric() || *c == '_')
                    .collect::<String>();
                let name = &name[..];
                if !KEYWORDS.contains_key(name) {
                    fail!("unknown identifier: {}", name);
                }
                skip += name.len();
                KEYWORDS[name].clone()
            }

            // TODO bindings
            _ => fail!("cannot tokenize: '{}' @ {} --> '{}'", c, i, &s[i..]),
        };

        data.push((i, token))
    }

    data.push((s.len(), Token::EOF));
    data
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore]
    fn test_keywords() {
        let input = r#"
            return 42;
        "#;

        let out = scan(&input);
        eprintln!("{:#?}", out);
    }
}
