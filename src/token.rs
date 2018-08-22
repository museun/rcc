use std::ops::{Index, IndexMut};

#[derive(PartialEq, Clone)]
pub enum Token {
    Add,           // +
    Sub,           // -
    Mul,           // *
    Div,           // /
    LogOr,         // ||
    LogAnd,        // &&
    LessThan,      // <
    GreaterThan,   // >
    Return,        // return
    If,            // if
    Else,          // else
    For,           // for
    Num(u32),      // n
    Ident(String), // name
    Int,           // int
    Assign,        // =
    OpenParen,     // (
    CloseParen,    // )
    OpenBrace,     // {
    CloseBrace,    // }
    Comma,         // ,
    Semicolon,     // ;
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
            '+' => Token::Add,
            '-' => Token::Sub,
            '*' => Token::Mul,
            '/' => Token::Div,
            ';' => Token::Semicolon,
            '=' => Token::Assign,
            '(' => Token::OpenParen,
            ')' => Token::CloseParen,
            '{' => Token::OpenBrace,
            '}' => Token::CloseBrace,
            ',' => Token::Comma,
            '<' => Token::LessThan,
            '>' => Token::GreaterThan,

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

use std::fmt;
impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Add => write!(f, "Add"),
            Token::Sub => write!(f, "Sub"),
            Token::Mul => write!(f, "Mul"),
            Token::Div => write!(f, "Div"),
            Token::LogOr => write!(f, "Or"),
            Token::LessThan => write!(f, "LessThan"),
            Token::GreaterThan => write!(f, "GreaterThan"),
            Token::LogAnd => write!(f, "And"),
            Token::Return => write!(f, "Return"),
            Token::If => write!(f, "If"),
            Token::Else => write!(f, "Else"),
            Token::For => write!(f, "For"),
            Token::Ident(name) => write!(f, "Ident({})", name),
            // types
            Token::Int => write!(f, "Int"),
            //
            Token::Assign => write!(f, "Assign"),
            Token::OpenParen => write!(f, "OpenParen"),
            Token::CloseParen => write!(f, "CloseParen"),
            Token::OpenBrace => write!(f, "OpenBrace"),
            Token::CloseBrace => write!(f, "CloseBrace"),
            Token::Comma => write!(f, "Comma"),
            Token::Semicolon => write!(f, "Semicolon"),
            Token::Num(n) => write!(f, "Num({})", n),
            Token::EOF => write!(f, "EOF"),
        }
    }
}

// impl From<Token> for Token {
//     fn from(c: Token) -> Token {
//         c
//     }
// }

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
        use token::Token::*;
        match c {
            '+' => Add,
            '-' => Sub,
            '*' => Mul,
            '/' => Div,
            '=' => Assign,
            '(' => OpenParen,
            ')' => CloseParen,
            '{' => OpenBrace,
            '}' => CloseBrace,
            '<' => LessThan,
            '>' => GreaterThan,
            ';' => Semicolon,
            ',' => Comma,
            _ => panic!("invalid char/token"),
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
            Token::LogOr => write!(f, "||"),
            Token::LogAnd => write!(f, "&&"),
            Token::LessThan => write!(f, "<"),
            Token::GreaterThan => write!(f, ">"),
            Token::Return => write!(f, "return"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::For => write!(f, "for"),
            Token::Ident(name) => write!(f, "{}", name),
            // types
            Token::Int => write!(f, "int"),
            //
            Token::Assign => write!(f, "="),
            Token::Semicolon => write!(f, ";"),
            Token::OpenParen => write!(f, "("),
            Token::CloseParen => write!(f, ")"),
            Token::OpenBrace => writeln!(f, "{{"),
            Token::CloseBrace => write!(f, "}}"),
            Token::Comma => write!(f, ","),
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
            if let Token::Semicolon = tok {
                writeln!(f)?
            }
        }
        Ok(())
    }
}
