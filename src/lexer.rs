use span::Span;
use tokens::{Token, Type};

use std::borrow::Cow;
use std::{char, str};

pub fn scan<'a>(file: &'a str, input: &'a str) -> Vec<(Span, Token)> {
    let mut data = vec![];

    let (mut line, mut row, mut skip) = (1, 0, 0);
    let mut stream = Input::new(&input);
    for (i, c) in input.char_indices() {
        row += 1;
        if c == '\n' {
            row = 0;
            line += 1;
        }

        if skip > 0 {
            skip -= 1;
            stream.advance(1);
            continue;
        }

        let span = Span::new(file, line, row);
        let mut error = None;
        'inner: for lexer in &LEXERS {
            match lexer(&mut stream.clone()) {
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

        stream.advance(1);
    }

    data
}

#[derive(Clone, Debug)]
struct Input<'a> {
    input: Cow<'a, str>,
    pos: usize,
}

impl<'a> Input<'a> {
    pub fn new(input: &'a str) -> Self {
        Input {
            input: input.into(),
            pos: 0,
        }
    }

    pub fn peek(&self) -> Option<char> {
        self.at(self.pos() + 1)
    }

    pub fn advance(&mut self, amt: usize) {
        self.pos += amt;
    }

    pub fn current(&self) -> char {
        self.at(self.pos()).unwrap()
    }

    pub fn at(&self, pos: usize) -> Option<char> {
        // is there a better way of doing this?
        self.input.chars().nth(pos)
    }

    pub fn move_to(&mut self, pos: usize) {
        self.pos = pos;
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    pub fn eof(&self) -> bool {
        self.pos >= self.input.len()
    }
}

impl<'a> Iterator for Input<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos + 1 == self.input.len() {
            return None;
        }
        self.pos += 1;
        Some(self.current())
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
    EndOfFile,
}

const LEXERS: [fn(&mut Input<'_>) -> State; 8] = [
    whitespace_lexer,
    comment_lexer,
    charliteral_lexer,
    char_lexer,
    string_lexer,
    symbol_lexer,
    digit_lexer,
    unknown_lexer,
];

fn unknown_lexer(_lexer: &mut Input<'_>) -> State {
    Error::UnknownToken.into()
}

fn whitespace_lexer(lexer: &mut Input<'_>) -> State {
    // need to check for \\\s+[\r|\n]
    let c = lexer.current();
    if !c.is_ascii_whitespace() && c != '\\' {
        return State::Yield;
    }

    let mut skip = 0;
    while let Some(c) = lexer.next() {
        if c == '\\' && lexer.peek().unwrap() == '\n' {
            skip += 1;
            continue;
        }
        if c.is_ascii_whitespace() {
            skip += 1;
        } else {
            break;
        }
    }

    State::Consume(skip)
}

fn comment_lexer(lexer: &mut Input<'_>) -> State {
    if '/' != lexer.current() {
        return State::Yield;
    }

    match lexer.next() {
        Some('/') => {
            let mut cont = false;
            let mut skip = 2;
            while let Some(p) = lexer.next() {
                match p {
                    '\\' => cont = true,
                    '\n' if cont => {
                        cont = false;
                        skip += 2;
                        continue;
                    }
                    '\n' => break,
                    _ => {
                        cont = false;
                        skip += 1;
                    }
                }
            }

            State::Produce(skip, Token::Comment(0, skip))
        }
        Some('*') => {
            lexer.advance(1);
            let mut skip = 2;
            while let Some(p) = lexer.next() {
                if p == '*' {
                    if let Some('/') = lexer.peek() {
                        skip += 2;
                        break;
                    }
                }
                skip += 1;
            }
            State::Produce(skip, Token::Comment(0, skip))
        }
        _ => State::Yield,
    }
}

fn digit_lexer(lexer: &mut Input<'_>) -> State {
    if !lexer.current().is_ascii_digit() {
        return State::Yield;
    }
    let digit = lexer.current();

    // TODO: this is ugly
    let mut skip = 0;
    let input = match digit {
        '0' => {
            // hex or octal
            match lexer.next().unwrap() {
                'x' | 'X' => {
                    let input = lexer
                        .take_while(char::is_ascii_hexdigit)
                        .filter_map(|c| c.to_digit(16))
                        .inspect(|_| skip += 1)
                        .fold(0, |a, n| 16 * a + n);
                    skip += 1;
                    input
                }
                _ => {
                    lexer.move_to(lexer.pos() - 2);
                    let input = lexer
                        .take_while(|&c| match c {
                            '0'...'7' => true,
                            _ => false,
                        }).filter_map(|c| c.to_digit(8))
                        .inspect(|_| skip += 1)
                        .fold(0, |a, n| 8 * a + n);
                    skip -= 1;
                    input
                }
            }
        }
        _ => {
            lexer.move_to(lexer.pos() - 1);
            let input = lexer
                .take_while(char::is_ascii_digit)
                .filter_map(|c| c.to_digit(10))
                .inspect(|_| skip += 1)
                .fold(0, |a, n| 10 * a + n);
            skip -= 1;
            input
        }
    };

    State::Produce(skip, Token::Num(input))
}

fn string_lexer(lexer: &mut Input<'_>) -> State {
    if '"' != lexer.current() {
        return State::Yield;
    }

    let mut buf = String::new();
    let (mut skip, mut escape) = (0, 0);

    for c in lexer {
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

fn char_lexer(lexer: &mut Input<'_>) -> State {
    if !is_first_char(lexer.current()) {
        return State::Yield;
    }

    let l = lexer.current();
    if let Some(p) = lexer.next() {
        if is_valid_char(l, p, None) {
            if let Some(e) = lexer.next() {
                if is_valid_char(l, p, Some(e)) {
                    return State::Produce(2, Token::MChar(l, p, Some(e)));
                }
            }
            return State::Produce(1, Token::MChar(l, p, None));
        }
    }

    State::Produce(0, Token::Char(l))
}

fn charliteral_lexer(lexer: &mut Input<'_>) -> State {
    if '\'' != lexer.current() {
        return State::Yield;
    }

    let ch = lexer.next().unwrap();
    if '\\' != ch {
        return State::Produce(3, Token::Num(ch as u32));
    }

    let ch = lexer.next();
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
        return State::Produce(3, Token::Num(ch as u32));
    }

    // '\x00'
    if ch == 'x' {
        let s = lexer.take(2).map(|c| c as u8).collect::<Vec<_>>();
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
        lexer.move_to(lexer.pos() - 1);
        let s = lexer.take(2).map(|c| c as u8).collect::<Vec<_>>();
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

    Error::InvalidEscape.into()
}

fn symbol_lexer(lexer: &mut Input<'_>) -> State {
    let p = lexer.current();
    if !p.is_alphabetic() && p != '_' {
        return State::Yield;
    }

    lexer.move_to(lexer.pos() - 1);
    let name = lexer
        .take_while(|&c| c.is_alphabetic() || c == '_')
        .collect::<String>();

    let skip = name.len() - 1;

    for sym in &SYMBOLS {
        if sym.0 == name {
            return State::Produce(skip, sym.1.clone());
        }
    }

    State::Produce(skip, Token::Ident(name))
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
pub fn is_first_char(left: char) -> bool {
    for (ch, _, _) in CHARACTERS.iter() {
        if left == *ch {
            return true;
        }
    }
    false
}

pub fn is_valid_char(l: char, m: char, e: Option<char>) -> bool {
    for (a, b, c) in CHARACTERS.iter() {
        if l == *a && Some(m) == *b && e == *c {
            return true;
        }
    }
    false
}
