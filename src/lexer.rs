use span::Span;
use tokens::{Token, Type};

use std::{char, str};

pub fn scan<'a>(
    file: &'a str,
    input: &'a str,
    lexers: &[&'static dyn Lexical],
) -> Vec<(Span, Token)> {
    let mut data = vec![];
    let mut skip = 0;

    let mut line = 1;
    let mut row = 0;
    for (i, c) in input.char_indices() {
        row += 1;
        if c == '\n' {
            row = 0;
            line += 1;
        }

        if skip > 0 {
            skip -= 1;
            continue;
        }

        let span = Span::new(file, line, row);

        let mut error = None;
        'inner: for lexer in lexers {
            let mut iter = input.chars().skip(i);
            match lexer.lex(&mut iter) {
                State::Yield => continue,
                State::Error(err) => error = Some(err),
                State::Consume(n) => skip += n,
                State::Produce(n, token) => {
                    let token = match token {
                        Token::Comment(_, b) => Token::Comment(i, b),
                        tok => tok,
                    };
                    skip += n;
                    data.push((span.clone(), token)); // why
                }
            }
            break 'inner;
        }

        match error {
            Some(Error::UnknownToken) => fail!("{}: unknown token found", span),
            _e => {}
        }
    }

    data
}

pub trait Lexical {
    fn lex(&self, &mut dyn Iterator<Item = char>) -> State {
        State::Yield
    }
}

pub enum State {
    Yield,
    Consume(usize),
    Produce(usize, Token),
    Error(Error),
}

impl From<Error> for State {
    fn from(err: Error) -> State {
        State::Error(err)
    }
}

pub enum Error {
    UnknownToken,
    InvalidEscape,
}

pub const LEXERS: [&'static dyn Lexical; 8] = [
    &WhitespaceLexer,
    &CommentLexer,
    &CharLiteralLexer,
    &CharLexer,
    &StringLexer,
    &SymbolLexer,
    &DigitLexer,
    &UnknownLexer,
];

struct WhitespaceLexer;
impl Lexical for WhitespaceLexer {
    fn lex(&self, lexer: &mut dyn Iterator<Item = char>) -> State {
        let mut iter = lexer.peekable();

        match iter.peek() {
            Some(' ') | Some('\t') | Some('\r') | Some('\n') => State::Consume(0),
            _ => State::Yield,
        }
    }
}

struct UnknownLexer;
impl Lexical for UnknownLexer {
    fn lex(&self, _lexer: &mut dyn Iterator<Item = char>) -> State {
        Error::UnknownToken.into()
    }
}

struct DigitLexer;
impl Lexical for DigitLexer {
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
impl Lexical for StringLexer {
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
impl Lexical for CharLexer {
    fn lex(&self, lexer: &mut dyn Iterator<Item = char>) -> State {
        let mut iter = lexer.peekable();

        if let Some(p) = iter.peek() {
            if !is_left_char(*p) {
                return State::Yield;
            }

            let l = iter.next().unwrap();
            if let Some(p) = iter.peek() {
                if is_valid_char(l, *p, None) {
                    let r = iter.next().unwrap();
                    if let Some(e) = iter.peek() {
                        if is_valid_char(l, r, Some(*e)) {
                            let e = iter.next();
                            return State::Produce(2, Token::MChar(l, r, e));
                        }
                    }
                    return State::Produce(1, Token::MChar(l, r, None));
                }
            }
            return State::Produce(0, Token::Char(l));
        }

        State::Yield
    }
}

struct CharLiteralLexer;
impl Lexical for CharLiteralLexer {
    fn lex(&self, lexer: &mut dyn Iterator<Item = char>) -> State {
        let mut iter = lexer.peekable();

        if let Some(p) = iter.peek() {
            if *p != '\'' {
                return State::Yield;
            }

            let mut iter = iter.skip(1).peekable();
            if let Some(ch) = iter.peek() {
                if *ch != '\\' {
                    return State::Produce(3, Token::Num(*ch as u32));
                }

                let mut iter = iter.skip(1);
                let ch = iter.next();
                if ch.is_none() {
                    return Error::InvalidEscape.into();
                };

                let ch = ch.unwrap();
                if let Some(ch) = match ch {
                    'a' => Some('\x07'),
                    'b' => Some('\x08'),
                    'f' => Some('\x0C'),
                    'n' => Some('\x0a'),
                    'r' => Some('\x0d'),
                    't' => Some('\x09'),
                    'v' => Some('\x0b'),
                    _ => None,
                } {
                    return State::Produce(4, Token::Num(ch as u32));
                }

                // '\x00'
                if ch == 'x' {
                    let s = iter.take(2).map(|c| c as u8).collect::<Vec<_>>();
                    if s.len() != 2 {
                        return Error::InvalidEscape.into();
                    }
                    match str::from_utf8(&s)
                        .ok()
                        .and_then(|i| u32::from_str_radix(i, 16).ok())
                    {
                        Some(c) => return State::Produce(5, Token::Num(c)),
                        _ => return Error::InvalidEscape.into(),
                    }
                }
                // '\000'
                if let Some(_ch) = match ch {
                    '0'...'3' => Some(ch),
                    _ => None,
                } {
                    let s = iter.take(3).map(|c| c as u8).collect::<Vec<_>>();
                    if s.len() != 3 {
                        return Error::InvalidEscape.into();
                    }
                    match str::from_utf8(&s)
                        .ok()
                        .and_then(|i| u32::from_str_radix(i, 8).ok())
                    {
                        Some(c) => return State::Produce(5, Token::Num(c)),
                        _ => return Error::InvalidEscape.into(),
                    }
                }

                return Error::InvalidEscape.into();
            }
        }

        State::Yield
    }
}

struct CommentLexer;
impl Lexical for CommentLexer {
    fn lex(&self, lexer: &mut dyn Iterator<Item = char>) -> State {
        let mut iter = lexer.peekable();

        if let Some('/') = iter.peek() {
            let _next = iter.next();

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
                    let mut iter = iter.skip(1).peekable();
                    while let Some(p) = iter.next() {
                        if p == '*' {
                            if let Some('/') = iter.peek() {
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
impl Lexical for SymbolLexer {
    fn lex(&self, lexer: &mut dyn Iterator<Item = char>) -> State {
        let mut iter = lexer.peekable();
        if let Some(p) = iter.peek() {
            if p.is_alphabetic() || *p == '_' {
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

const SYMBOLS: [(&str, Token); 15] = [
    ("int", Token::Type(Type::Int)),
    ("char", Token::Type(Type::Char)),
    ("void", Token::Type(Type::Void)),
    //
    ("for", Token::For),
    ("if", Token::If),
    ("else", Token::Else),
    ("do", Token::Do),
    ("while", Token::While),
    ("return", Token::Return),
    ("sizeof", Token::Sizeof),
    ("_Alignof", Token::Alignof),
    ("extern", Token::Extern),
    ("struct", Token::Struct),
    ("typedef", Token::Typedef),
    ("break", Token::Break),
];

const CHARACTERS: [(char, Option<char>, Option<char>); 45] = [
    ('&', Some('&'), None),
    ('|', Some('|'), None),
    //
    ('=', Some('='), None),
    ('!', Some('='), None),
    ('>', Some('='), None),
    ('<', Some('='), None),
    //
    ('<', Some('<'), Some('=')),
    ('>', Some('>'), Some('=')),
    ('*', Some('='), None),
    ('/', Some('='), None),
    ('%', Some('='), None),
    ('+', Some('='), None),
    ('-', Some('='), None),
    ('&', Some('='), None),
    ('^', Some('='), None),
    ('|', Some('='), None),
    //
    ('<', Some('<'), None),
    ('>', Some('>'), None),
    //
    ('+', Some('+'), None),
    ('-', Some('-'), None),
    //
    ('-', Some('>'), None),
    //
    ('?', None, None), // ? :
    (':', None, None), // ? :
    //
    ('!', None, None),
    ('+', None, None),
    ('-', None, None),
    ('*', None, None),
    ('/', None, None),
    (';', None, None),
    ('=', None, None),
    ('(', None, None),
    (')', None, None),
    ('{', None, None),
    ('}', None, None),
    ('[', None, None),
    (']', None, None),
    (',', None, None),
    ('<', None, None),
    ('>', None, None),
    ('&', None, None),
    ('.', None, None),
    ('|', None, None),
    ('^', None, None),
    ('%', None, None),
    ('~', None, None),
];

// TODO: this shouldn't be public
pub fn is_left_char(left: char) -> bool {
    for (ch, _, _) in CHARACTERS.iter() {
        if left == *ch {
            return true;
        }
    }
    false
}

pub fn is_valid_char(l: char, r: char, end: Option<char>) -> bool {
    for (a, b, c) in CHARACTERS.iter() {
        if l == *a && Some(r) == *b && end == *c {
            return true;
        }
    }
    false
}
