#![allow(dead_code, unused_variables)] //go away clippy
use super::*;

pub fn scan(input: &str, lexers: &[&'static dyn Lexical<'static>]) -> Vec<(usize, Token)> {
    let mut data = vec![];
    let mut skip = 0;

    for (i, c) in input.char_indices() {
        if skip > 0 {
            skip -= 1;
            continue;
        }

        let mut error = None;

        'inner: for lexer in lexers {
            let mut iter = input.chars().skip(i);
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

pub const LEXERS: [&'static dyn Lexical; 7] = [
    &WhitespaceLexer,
    &CommentLexer,
    &CharLexer,
    &StringLexer,
    &SymbolLexer,
    &DigitLexer,
    &UnknownLexer,
];

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
    ("int", Token::Type(TokType::Int)),
    ("char", Token::Type(TokType::Char)),
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

// TODO: this shouldn't be public
pub(crate) fn is_left_char(left: char) -> bool {
    for (ch, _) in &CHARACTERS {
        if left == *ch {
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
