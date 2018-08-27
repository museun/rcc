#![allow(dead_code, unused_variables)] //go away clippy
use super::*;
use std::ops::{Index, IndexMut};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    MChar(char, char), // double-width chars
    Char(char),        // single-width others
    Num(u32),          // constant
    Ident(String),     // name
    Type(LexType),     // types
    Str(String),       // string

    If,   // if
    Else, // else
    For,  // for
    // LogOr,   // ||
    // LogAnd,  // &&
    // Equals,  // ==
    // NEquals, // !=
    Do,     // do
    While,  // while
    Return, // return
    Sizeof, // sizeof
    Extern, // extern

    Unknown, // for errors

    Comment(usize, usize), // comment. start, end

    EOF,
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

impl From<&'static str> for Token {
    fn from(c: &'static str) -> Token {
        match c {
            "else" => Token::Else,
            "==" => Token::MChar('=', '='),
            "!=" => Token::MChar('!', '='),
            _ => panic!("invalid str/token"),
        }
    }
}

impl From<char> for Token {
    fn from(c: char) -> Token {
        if is_left_char(c) {
            return Token::Char(c);
        }

        panic!("invalid char/token")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum LexType {
    Char,
    Int,
}

#[derive(Debug, Clone)]
pub struct Tokens<'a> {
    data: Vec<(usize, Token)>,
    input: &'a str,
    pos: usize,
}

impl<'a> Tokens<'a> {
    pub fn tokenize(input: &'a str) -> Self {
        let mut lexer = Lexer::new(input);
        let data = lexer.scan(&[
            &WhitespaceLexer,
            &CommentLexer,
            &CharLexer,
            &StringLexer,
            &SymbolLexer,
            &DigitLexer,
            &UnknownLexer,
        ]);

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

    pub fn pos(&self) -> usize {
        self.pos
    }

    pub fn next_token(&mut self) -> Option<&(usize, Token)> {
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
    pub fn peek(&mut self) -> Option<&(usize, Token)> {
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

pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input, pos: 0 }
    }

    pub fn scan(&mut self, lexers: &[&'a dyn Lexical<'a>]) -> Vec<(usize, Token)> {
        let mut data = vec![];
        let mut skip = 0;
        for (i, c) in self.input.char_indices() {
            if skip > 0 {
                skip -= 1;
                continue;
            }

            let mut error = None;;

            'inner: for lexer in lexers {
                let mut iter = self.input.chars().skip(i);
                match lexer.lex(&mut iter) {
                    State::Yield => {}
                    State::Error => {
                        error = Some("error");
                        break 'inner;
                    }
                    State::Consume(n) => {
                        skip += n;
                        break 'inner;
                    }
                    State::Produce(n, token) => {
                        let token = match token {
                            Token::Comment(_, b) => Token::Comment(i, b),
                            tok => tok,
                        };
                        skip += n;
                        data.push((i, token));
                        break 'inner;
                    }
                }
            }

            if let Some(err) = error {
                fail!(
                    "error at {}. {:#?}",
                    i,
                    data.iter().map(|(_, d)| d).collect::<Vec<_>>()
                );
            }
        }

        data
    }
}

struct WhitespaceLexer;
impl<'a> Lexical<'a> for WhitespaceLexer {
    fn lex(&self, lexer: &mut dyn Iterator<Item = char>) -> State {
        let mut iter = lexer.peekable();

        match iter.peek() {
            Some(' ') | Some('\t') | Some('\r') | Some('\n') => State::Consume(0),
            _ => State::Yield,
        }
    }
}

struct UnknownLexer;
impl<'a> Lexical<'a> for UnknownLexer {
    fn lex(&self, lexer: &mut dyn Iterator<Item = char>) -> State {
        State::Error
    }
}

struct DigitLexer;
impl<'a> Lexical<'a> for DigitLexer {
    fn lex(&self, lexer: &mut dyn Iterator<Item = char>) -> State {
        let mut iter = lexer.peekable();
        if let Some(next) = iter.peek() {
            if next.is_ascii_digit() {
                let mut skip = 0;
                let input: u32 = iter
                    .take_while(char::is_ascii_digit)
                    .filter_map(|c| c.to_digit(10))
                    .inspect(|_| skip += 1)
                    .fold(0, |a, n| 10 * a + n);
                skip -= 1;
                return State::Produce(skip, Token::Num(input));
            }
        }

        State::Yield
    }
}

struct StringLexer;
impl<'a> Lexical<'a> for StringLexer {
    fn lex(&self, lexer: &mut dyn Iterator<Item = char>) -> State {
        let mut iter = lexer.peekable();
        match iter.peek() {
            Some('"') => {
                let mut buf = String::new();
                let mut skip = 0;
                let mut escape = 0;

                for c in iter.skip(1) {
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

                State::Produce(skip, Token::Str(buf))
            }
            _ => State::Yield,
        }
    }
}

struct CharLexer;
impl<'a> Lexical<'a> for CharLexer {
    fn lex(&self, lexer: &mut dyn Iterator<Item = char>) -> State {
        let mut iter = lexer.peekable();

        if let Some(p) = iter.peek() {
            if !is_left_char(*p) {
                return State::Yield;
            }

            let l = iter.next().unwrap();
            if let Some(p) = iter.peek() {
                if is_valid_char(l, *p) {
                    let r = iter.next().unwrap();
                    return State::Produce(1, Token::MChar(l, r));
                }
            }
            return State::Produce(0, Token::Char(l));
        }

        State::Yield
    }
}

struct CharLiteralLexer;
impl<'a> Lexical<'a> for CharLiteralLexer {
    fn lex(&self, lexer: &mut dyn Iterator<Item = char>) -> State {
        let mut iter = lexer.peekable();

        if let Some(p) = iter.peek() {
            if *p != '\'' {
                return State::Yield;
            }

            if let Some(esc) = is_escape(*p) {
                return State::Produce(0, Token::Char(esc));
            }
        }

        State::Yield
    }
}

struct CommentLexer;
impl<'a> Lexical<'a> for CommentLexer {
    fn lex(&self, lexer: &mut dyn Iterator<Item = char>) -> State {
        let mut iter = lexer.peekable();

        if let Some('/') = iter.peek() {
            let next = iter.next();

            return match iter.peek() {
                Some('/') => {
                    let skip = iter
                        .skip(1)
                        .take_while(|&c| c != '\n')
                        .fold(2, |j, _| j + 1);
                    State::Produce(skip, Token::Comment(0, skip))
                }
                Some('*') => {
                    let mut skip = 2;
                    let mut iter = iter.skip(1).enumerate().peekable();
                    while let Some((i, p)) = iter.next() {
                        if p == '*' {
                            if let Some((_, '/')) = iter.peek() {
                                skip += 2;
                                break;
                            }
                        }
                        skip += 1;
                    }
                    State::Produce(skip, Token::Comment(0, skip))
                }
                _ => State::Yield,
            };
        }
        State::Yield
    }
}

struct SymbolLexer;
impl<'a> Lexical<'a> for SymbolLexer {
    fn lex(&self, lexer: &mut dyn Iterator<Item = char>) -> State {
        let mut iter = lexer.peekable();
        if let Some(p) = iter.peek() {
            if p.is_alphabetic() {
                let name = iter
                    .take_while(|&c| c.is_alphabetic() || c == '_')
                    .collect::<String>();
                let skip = name.len() - 1;

                for sym in &SYMBOLS {
                    if sym.0 == name {
                        return State::Produce(skip, sym.1.clone());
                    }
                }

                return State::Produce(skip, Token::Ident(name));
            }
        }

        State::Yield
    }
}

const SYMBOLS: [(&str, Token); 10] = [
    ("int", Token::Type(LexType::Int)),
    ("char", Token::Type(LexType::Char)),
    ("for", Token::For),
    ("if", Token::If),
    ("else", Token::Else),
    ("do", Token::Do),
    ("while", Token::While),
    ("return", Token::Return),
    ("sizeof", Token::Sizeof),
    ("extern", Token::Extern),
];

const CHARACTERS: [(char, Option<char>); 20] = [
    ('&', Some('&')),
    ('|', Some('|')),
    ('=', Some('=')),
    ('!', Some('=')),
    ('+', None),
    ('-', None),
    ('*', None),
    ('/', None),
    (';', None),
    ('=', None),
    ('(', None),
    (')', None),
    ('{', None),
    ('}', None),
    ('[', None),
    (']', None),
    (',', None),
    ('<', None),
    ('>', None),
    ('&', None),
];

fn is_left_char(l: char) -> bool {
    for (a, _) in &CHARACTERS {
        if l == *a {
            return true;
        }
    }
    false
}

fn is_valid_char(l: char, r: char) -> bool {
    for (a, b) in &CHARACTERS {
        if l == *a && Some(r) == *b {
            return true;
        }
    }
    false
}

const VALID_ESCAPE: [(u8, char); 7] = [
    (b'\x07', 'a'),
    (b'\x08', 'b'),
    (b'\x0C', 'f'),
    (b'\x0a', 'n'),
    (b'\x0d', 'r'),
    (b'\x09', 't'),
    (b'\x0b', 'v'),
];

fn is_escape(c: char) -> Option<char> {
    for (k, v) in &VALID_ESCAPE {
        if *k == c as u8 {
            // UTF8: ??
            return Some(*v);
        }
    }

    is_octal_escape(c).or_else(|| is_hex_escape(c))
}

fn is_octal_escape(c: char) -> Option<char> {
    // TODO implement
    None
}

fn is_hex_escape(c: char) -> Option<char> {
    // TODO implement
    None
}

pub trait Lexical<'a> {
    fn lex(&self, lexer: &mut dyn Iterator<Item = char>) -> State {
        State::Yield
    }
}

pub enum State {
    Yield,
    Consume(usize),
    Produce(usize, Token),
    Error,
}

use std::fmt;
impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Char(c) => write!(f, "{} : Char", c),
            Token::MChar(l, r) => write!(f, "{}{} : MChar", l, r),
            Token::Num(n) => write!(f, "{} : Num", n),
            Token::Ident(name) => write!(f, "{} : Ident", name),
            Token::Type(ty) => write!(f, "{:?} : Type", ty),
            Token::Str(s) => write!(f, "\"{}\" : String", s),
            Token::If => write!(f, "If"),
            Token::Else => write!(f, "Else"),
            Token::For => write!(f, "For"),
            Token::Extern => write!(f, "Extern"),
            Token::Do => write!(f, "Do"),
            Token::While => write!(f, "While"),
            Token::Return => write!(f, "Return"),
            Token::Sizeof => write!(f, "Sizeof"),
            Token::Comment(start, end) => write!(f, "Comment({}, {})", start, end),
            Token::EOF => write!(f, "EOF"),
            _ => Ok(()), // don't print unknowns?
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

impl<'a> fmt::Display for Tokens<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, (pos, tok)) in self.data.iter().enumerate() {
            write!(
                f,
                "{},{}{: >2}",
                wrap_color!(Color::Magenta {}, "{: >4}", i),
                wrap_color!(Color::Cyan {}, "{: <4}", pos),
                "",
            )?;
            writeln!(f, "{}", tok)?
        }
        Ok(())
    }
}
