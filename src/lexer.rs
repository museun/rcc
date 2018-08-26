use super::*;
use std::ops::{Index, IndexMut};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Char(char),    // all others
    Num(u32),      // constant
    Ident(String), // name
    Type(LexType), // types
    Str(String),   // string

    If,     // if
    Else,   // else
    For,    // for
    LogOr,  // ||
    LogAnd, // &&
    Return, // return
    Sizeof, // sizeof

    EOF,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LexType {
    Char,
    Int,
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    data: Vec<(usize, Token)>,
    input: &'a str,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn tokenize(input: &'a str) -> Self {
        Lexer {
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

impl<'a> Index<usize> for Lexer<'a> {
    type Output = Token;

    fn index(&self, p: usize) -> &Self::Output {
        &self.data[p].1
    }
}

impl<'a> IndexMut<usize> for Lexer<'a> {
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
    symbols.push(("int", Token::Type(LexType::Int)));
    symbols.push(("char", Token::Type(LexType::Char)));
    symbols.push(("for", Token::For));
    symbols.push(("if", Token::If));
    symbols.push(("else", Token::Else));
    symbols.push(("return", Token::Return));
    symbols.push(("sizeof", Token::Sizeof));
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

            // string literals
            '"' => {
                let mut buf = String::new();
                let mut escape = 0;
                for c in s[i + 1..].chars() {
                    skip += 1;
                    if c == '\\' {
                        escape += 1;
                        continue;
                    }
                    if escape & 1 == 1 {
                        buf.push_str("\\");
                        buf.push(c);
                        escape = 0;
                        continue;
                    }
                    if escape == 0 && c == '"' {
                        break;
                    }
                    buf.push(c);
                }

                Token::Str(buf)
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
                out.or_else(|| {
                    if is_valid_char(c) {
                        Some(Token::Char(c))
                    } else {
                        // TODO make this more readable
                        eprintln!("{:?}", data);
                        fail!("unknown punctuation '{}' @ {} --> '{}'", c, i, &s[i..]);
                    }
                }).unwrap()
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

                out.or_else(|| Some(Token::Ident(name))).unwrap()
            }

            // TODO make this more readable
            _ => fail!("cannot tokenize: '{}' @ {} --> '{}'", c, i, &s[i..]),
        };

        data.push((i, token))
    }

    data.push((s.len(), Token::EOF));
    data
}

fn is_valid_char(c: char) -> bool {
    const CHARS: [char; 16] = [
        '+', '-', '*', '/', //
        ';', '=', '(', ')', //
        '{', '}', '[', ']', //
        ',', '<', '>', '&', //
    ];
    CHARS.contains(&c)
}

impl Token {
    /// this panics if its not a Token::Char
    pub(crate) fn get_char(&self) -> &char {
        match self {
            Token::Char(c) => c,
            _ => unreachable!(),
        }
    }
}

use std::fmt;
impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Char(c) => write!(f, "Char({})", c),
            Token::Num(n) => write!(f, "Num({})", n),
            Token::Ident(name) => write!(f, "Ident({})", name),
            Token::Type(ty) => write!(f, "Type({:?})", ty),
            Token::Str(s) => write!(f, "String(\"{}\")", s),
            Token::If => write!(f, "If"),
            Token::Else => write!(f, "Else"),
            Token::For => write!(f, "For"),
            Token::LogOr => write!(f, "Or"),
            Token::LogAnd => write!(f, "And"),
            Token::Return => write!(f, "Return"),
            Token::Sizeof => write!(f, "Sizeof"),
            Token::EOF => write!(f, "EOF"),
        }
    }
}

impl fmt::Display for LexType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LexType::Char => write!(f, "Char"),
            LexType::Int => write!(f, "Int"),
        }
    }
}

impl From<&'static str> for Token {
    fn from(c: &'static str) -> Token {
        match c {
            "else" => Token::Else,
            _ => panic!("invalid str/token"),
        }
    }
}

impl From<char> for Token {
    fn from(c: char) -> Token {
        if is_valid_char(c) {
            return Token::Char(c);
        }

        panic!("invalid char/token")
    }
}

impl<'a> fmt::Display for Lexer<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (pos, tok) in &self.data {
            write!(f, "{}", wrap_color!(Color::Cyan {}, "{: >4}>\t", pos));
            match tok {
                Token::Char(c) => writeln!(f, "{}", c)?,
                tok => writeln!(f, "{}", tok)?,
            }
        }
        Ok(())
    }
}
