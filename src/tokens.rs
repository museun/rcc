use lexer::{self, LEXERS};
use span::Span;
use util::*;

use std::fmt;
use std::ops::{Index, IndexMut};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    MChar(char, char, Option<char>), // double/triple-width chars

    Char(char),    // single-width others
    Num(u32),      // constant
    Ident(String), // name
    Type(Type),    // types
    Str(String),   // string

    If,      // if
    Else,    // else
    For,     // for
    Do,      // do
    While,   // while
    Return,  // return
    Sizeof,  // sizeof
    Alignof, // _Alignof
    Extern,  // extern
    Struct,  // struct
    Typedef, // typedef
    Break,   // break

    Comment(usize, usize), // comment. start, end

    EOF,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Char,
    Int,
    Void,
}

#[derive(Debug, Clone)]
pub struct Tokens<'a> {
    data: Vec<(Span<'a>, Token)>,
    input: &'a str,
    pos: usize,
}

impl<'a> Tokens<'a> {
    pub fn tokenize(file: &'a str, input: &'a str) -> Self {
        let data = lexer::scan(file, input, &LEXERS);

        Tokens {
            data,
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

    pub fn pos(&self) -> Span {
        self.data[self.pos].0
    }

    pub fn next_token(&mut self) -> Option<&(Span, Token)> {
        loop {
            match self.data.get(self.pos) {
                Some((_, Token::Comment(_, _))) => self.pos += 1,
                Some(tok) => {
                    self.pos += 1;
                    return Some(tok);
                }
                tok => return tok,
            }
        }
    }

    /// this also eats comments
    pub fn peek(&mut self) -> Option<&(Span, Token)> {
        loop {
            match self.data.get(self.pos) {
                Some((_, Token::Comment(_, _))) => self.pos += 1,
                Some(tok) => return Some(tok),
                tok => return tok,
            }
        }
    }

    pub fn advance(&mut self) {
        self.pos += 1;
    }

    pub fn input_at(&self, pos: usize) -> &'a str {
        &self.input[pos..]
    }

    pub fn tokens_at(&self, pos: usize) -> impl Iterator<Item = &(Span, Token)> {
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
    fn index_mut(&mut self, p: usize) -> &mut Self::Output {
        &mut self.data[p].1
    }
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

impl PartialEq<char> for Token {
    fn eq(&self, other: &char) -> bool {
        if let Token::Char(c) = self {
            return c == other;
        }
        false
    }
}

impl<'a> From<&'a str> for Token {
    fn from(input: &'a str) -> Token {
        debug_assert!(input.len() >= 2 && input.len() <= 3);

        let mut chars = input.chars();
        let (left, middle, right) = (chars.next().unwrap(), chars.next().unwrap(), chars.next());

        if !lexer::is_valid_char(left, middle, right) {
            panic!("invalid str/token")
        }

        Token::MChar(left, middle, right)
    }
}

impl From<char> for Token {
    fn from(c: char) -> Token {
        if lexer::is_left_char(c) {
            return Token::Char(c);
        }

        panic!("invalid char/token")
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Char(c) => write!(f, "{} : Char", c),
            Token::MChar(l, r, None) => write!(f, "{}{} : MChar", l, r),
            Token::MChar(l, r, Some(e)) => write!(f, "{}{}{} : MChar", l, r, e),
            Token::Num(n) => write!(f, "{} : Num", n),
            Token::Ident(name) => write!(f, "{} : Ident", name),
            Token::Type(ty) => write!(f, "{} : Type", ty),
            Token::Str(s) => write!(f, "\"{}\" : String", s),

            Token::If => write!(f, "If"),
            Token::Else => write!(f, "Else"),
            Token::For => write!(f, "For"),
            Token::Extern => write!(f, "Extern"),
            Token::Do => write!(f, "Do"),
            Token::While => write!(f, "While"),
            Token::Return => write!(f, "Return"),
            Token::Sizeof => write!(f, "Sizeof"),
            Token::Alignof => write!(f, "Alignof"),
            Token::Struct => write!(f, "Struct"),
            Token::Typedef => write!(f, "Typedef"),
            Token::Break => write!(f, "Break"),

            Token::Comment(start, end) => write!(f, "Comment({}, {})", start, end),

            Token::EOF => write!(f, "EOF"),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Char => write!(f, "Char"),
            Type::Int => write!(f, "Int"),
            Type::Void => write!(f, "Void"),
        }
    }
}

impl<'a> fmt::Display for Tokens<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let max = self
            .data
            .iter()
            .map(|(pos, _)| pos.total_width())
            .max()
            .expect("to get max width");

        for (i, (pos, tok)) in self.data.iter().enumerate() {
            write!(
                f,
                "{}{: <width$}{}{: >4}",
                wrap_color!(Color::Cyan {}, "{}", pos),
                "",
                wrap_color!(Color::Magenta {}, "{: >5}", i),
                "",
                width = max - pos.total_width()
            )?;
            writeln!(f, "{}", tok)?
        }
        Ok(())
    }
}
