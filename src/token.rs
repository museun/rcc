use std::{
    collections::HashMap,
    ops::{Index, IndexMut},
};

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, Token> = {
        let mut map = HashMap::new();
        map.insert("return", Token::Ret);
        map.insert("if", Token::If);
        map
    };
}

#[derive(PartialEq, Clone)]
pub enum Token {
    Add,
    Sub,
    Mul,
    Div,
    Ret,
    If,
    Ident(String), // trust me on the heap allocation
    Assign,
    OpenParen,
    CloseParen,
    EOS, // end of statement (expression? what are they called in legallese C)
    Num(u32),
    EOF,
}

#[derive(Clone)]
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

    pub fn peek(&self) -> Option<&(usize, Token)> {
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
            ';' => Token::EOS,
            '=' => Token::Assign,
            '(' => Token::OpenParen,
            ')' => Token::CloseParen,

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

            // identifiers
            c if c.is_alphabetic() => {
                let name = s[i..]
                    .chars()
                    .take_while(|c| c.is_alphanumeric() || *c == '_')
                    .collect::<String>();
                skip += name.len() - 1;

                if KEYWORDS.contains_key(name.as_str()) {
                    KEYWORDS[name.as_str()].clone()
                } else {
                    Token::Ident(name)
                }
            }

            _ => fail!("cannot tokenize: '{}' @ {} --> '{}'", c, i, &s[i..]),
        };

        data.push((i, token))
    }

    data.push((s.len(), Token::EOF));
    data
}

use std::fmt;
impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Add => write!(f, "Add"),
            Token::Sub => write!(f, "Sub"),
            Token::Mul => write!(f, "Mul"),
            Token::Div => write!(f, "Div"),
            Token::Ret => write!(f, "Ret"),
            Token::If => write!(f, "If"),
            Token::Ident(name) => write!(f, "Ident({})", name),
            Token::Assign => write!(f, "Assign"),
            Token::OpenParen => write!(f, "OpenParen"),
            Token::CloseParen => write!(f, "CloseParen"),
            Token::EOS => write!(f, "EOS"),
            Token::Num(n) => write!(f, "Num({})", n),
            Token::EOF => write!(f, "EOF"),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Add => write!(f, "+"),
            Token::Sub => write!(f, "-"),
            Token::Mul => write!(f, "*"),
            Token::Div => write!(f, "/"),
            Token::Ret => write!(f, "return"),
            Token::If => write!(f, "if"),
            Token::Ident(name) => write!(f, "{}", name),
            Token::Assign => write!(f, "="),
            Token::EOS => write!(f, ";"),
            Token::OpenParen => write!(f, "("),
            Token::CloseParen => write!(f, ")"),
            Token::Num(n) => write!(f, "{}", n),
            Token::EOF => write!(f, "▯"),
        }
    }
}

impl<'a> fmt::Debug for Tokens<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (pos, tok) in &self.data {
            writeln!(f, "{}> {:?}", pos, tok)?;
        }
        Ok(())
    }
}

impl<'a> fmt::Display for Tokens<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (_pos, tok) in &self.data {
            write!(f, "{} ", tok)?;
            if let Token::EOS = tok {
                writeln!(f)?
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore]
    fn test_keywords() {
        let input = r#"
            int a = 4; return a;
        "#;

        let out = Tokens::tokenize(&input);
        eprintln!("{:?}", out);
    }
}
