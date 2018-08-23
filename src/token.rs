use super::*;
use std::ops::{Index, IndexMut};

#[derive(PartialEq, Clone)]
pub enum Token {
    Char(char),    // all others
    Num(u32),      // n
    Ident(String), // name
    Int,           // int
    If,            // if
    Else,          // else
    For,           // for
    LogOr,         // ||
    LogAnd,        // &&
    Return,        // return

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
            self.pos += 1; // why isn't this working
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

    pub fn tokens_at(&self, pos: usize) -> impl Iterator<Item = &(usize, Token)> {
        self.data.iter().skip(pos)
    }
}

// TODO impl range

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

impl PartialEq<char> for Token {
    fn eq(&self, other: &char) -> bool {
        if let Token::Char(c) = self {
            return c == other;
        }
        false
    }
}

fn scan(s: &str) -> Vec<(usize, Token)> {
    let mut symbols = vec![];
    symbols.push(("if", Token::If));
    symbols.push(("else", Token::Else));
    symbols.push(("int", Token::Int));
    symbols.push(("return", Token::Return));
    symbols.push(("for", Token::For));
    symbols.push(("&&", Token::LogAnd));
    symbols.push(("||", Token::LogOr));

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
            '+' | '-' | '*' | '/' | ';' | '=' | '(' | ')' | '{' | '}' | ',' | '<' | '>' => {
                Token::Char(c)
            }

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

            // multi-character token
            c if c.is_ascii_punctuation() => {
                let mut out = None;
                for symbol in &symbols {
                    if s[i..].starts_with(symbol.0) {
                        skip += symbol.0.len() - 1;
                        out = Some(symbol.1.clone())
                    }
                }
                if out.is_none() {
                    fail!("unknown punctuation: '{}' @ {} --> '{}'", c, i, &s[i..])
                }
                out.unwrap()
            }

            // identifiers
            c if c.is_alphabetic() => {
                let name = s[i..]
                    .chars()
                    .take_while(|c| c.is_alphanumeric() || *c == '_')
                    .collect::<String>();
                skip += name.len() - 1;

                let mut out = None;
                for symbol in &symbols {
                    if symbol.0 == name {
                        out = Some(symbol.1.clone());
                    }
                }
                if out.is_none() {
                    Token::Ident(name)
                } else {
                    out.unwrap()
                }
            }

            _ => fail!("cannot tokenize: '{}' @ {} --> '{}'", c, i, &s[i..]),
        };

        data.push((i, token))
    }

    data.push((s.len(), Token::EOF));
    data
}

impl Token {
    // this panics if its not a Token::Char
    pub(crate) fn get_char(&self) -> &char {
        match self {
            Token::Char(c) => c,
            _ => unreachable!(),
        }
    }
}

use std::fmt;
impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Char(c) => write!(f, "Char({})", c),
            Token::Num(n) => write!(f, "Num({})", n),
            Token::Ident(name) => write!(f, "Ident({})", name),
            Token::Int => write!(f, "Int"),
            Token::If => write!(f, "If"),
            Token::Else => write!(f, "Else"),
            Token::For => write!(f, "For"),
            Token::LogOr => write!(f, "Or"),
            Token::LogAnd => write!(f, "And"),
            Token::Return => write!(f, "Return"),
            Token::EOF => write!(f, "EOF"),
        }
    }
}

impl From<&'static str> for Token {
    fn from(c: &'static str) -> Token {
        use token::Token::*;
        match c {
            "else" => Else,
            _ => panic!("invalid str/token"),
        }
    }
}

impl From<char> for Token {
    fn from(c: char) -> Token {
        match c {
            '+' | '-' | '*' | '/' | '=' | '(' | ')' | '{' | '}' | '<' | '>' | ';' | ',' => {
                Token::Char(c)
            }
            _ => panic!("invalid char/token"),
        }
    }
}

impl<'a> fmt::Debug for Tokens<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (pos, tok) in &self.data {
            write!(f, "{}", wrap_color!(Color::Cyan {}, "{: >4}>\t", pos));
            match tok {
                Token::Char(c) => writeln!(f, "{}", c)?,
                tok => writeln!(f, "{:?}", tok)?,
            }
        }
        Ok(())
    }
}
