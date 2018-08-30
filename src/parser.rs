use super::*;
use kind::Kind;
use node::{Comp, Node};
use span::Span;
use tokens::{Token, Tokens, Type as TokType};
use types::Type;

use std::collections::HashMap;

#[derive(Debug, Clone)]
struct Environment {
    tags: HashMap<String, Vec<Node>>,
    prev: Option<Box<Environment>>,
}

impl Environment {
    pub fn new(env: Option<Environment>) -> Self {
        Self {
            tags: HashMap::new(),
            prev: env.and_then(|env| Some(Box::new(env))),
        }
    }
}

pub struct Parser {
    env: Environment,
}

impl Parser {
    pub fn parse(mut tokens: Tokens) -> Vec<Node> {
        let tokens = &mut tokens;

        let mut this = Parser {
            env: Environment::new(None),
        };

        let mut nodes = vec![];
        while let Some((_, tok)) = tokens.peek() {
            if *tok == Token::EOF {
                break;
            }
            nodes.push(this.root(tokens))
        }
        nodes
    }

    fn root(&mut self, tokens: &mut Tokens) -> Node {
        let is_extern = consume(tokens, Token::Extern);
        let ty = self.type_(tokens);
        let name = self.ident(tokens);

        // functions
        if consume(tokens, '(') {
            let mut args = vec![];
            if !consume(tokens, ')') {
                args.push(Kind::make(self.param(tokens)));
                while consume(tokens, ',') {
                    args.push(Kind::make(self.param(tokens)));
                }
                expect_token(tokens, ')');
            }
            expect_token(tokens, '{');

            return Node::Func {
                ty: Rc::new(RefCell::new(ty)),
                body: Kind::make(self.compound(tokens)),
                name,
                args,
                stacksize: 0,
                globals: vec![],
            };
        }

        let data = if is_extern { 0 } else { types::size_of(&ty) };

        let node = Node::Vardef {
            ty: Rc::new(RefCell::new(self.read_array(tokens, ty))),
            name,
            init: Kind::empty(),
            offset: 0,
            data,
            is_extern,
        };

        expect_token(tokens, ';');
        node
    }

    fn param(&mut self, tokens: &mut Tokens) -> Node {
        let ty = self.type_(tokens);
        let name = self.ident(tokens);
        let init = if consume(tokens, '=') {
            Kind::make(self.assign(tokens))
        } else {
            Kind::empty()
        };

        let data = types::size_of(&ty);
        Node::Vardef {
            name,
            init,
            offset: 0,
            ty: Rc::new(RefCell::new(ty)),
            data,
            is_extern: false,
        }
    }

    fn compound(&mut self, tokens: &mut Tokens) -> Node {
        let newenv = Environment::new(Some(self.env.clone()));
        self.env = newenv;

        let mut stmts = vec![];
        while !consume(tokens, '}') {
            stmts.push(Kind::make(self.statement(tokens)))
        }

        // nice
        self.env = *(self.env.prev.as_ref().unwrap()).clone();

        Node::Compound { stmts }
    }

    fn statement(&mut self, tokens: &mut Tokens) -> Node {
        let (pos, next) = tokens.peek().expect("token for statement");

        match next {
            Token::Type(_) | Token::Struct => self.declaration(tokens),
            Token::If => {
                tokens.advance();
                expect_token(tokens, '(');
                let cond = Kind::make(self.assign(tokens));

                expect_token(tokens, ')');
                let body = Kind::make(self.statement(tokens));

                let else_ = if consume(tokens, "else") {
                    Kind::make(self.statement(tokens))
                } else {
                    Kind::empty()
                };

                Node::If { cond, body, else_ }
            }
            Token::For => {
                tokens.advance();
                expect_token(tokens, '(');
                let init = if is_typename(tokens) {
                    self.declaration(tokens)
                } else {
                    self.expression(tokens)
                };

                let cond = self.assign(tokens);
                expect_token(tokens, ';');

                let step = Node::Expression {
                    expr: Kind::make(self.assign(tokens)),
                };

                expect_token(tokens, ')');

                let body = self.statement(tokens);
                Node::For {
                    init: Kind::make(init),
                    cond: Kind::make(cond),
                    step: Kind::make(step),
                    body: Kind::make(body),
                }
            }

            Token::While => {
                tokens.advance();

                expect_token(tokens, '(');
                let cond = self.assign(tokens);

                expect_token(tokens, ')');
                let body = self.statement(tokens);

                Node::For {
                    init: Kind::empty(),
                    cond: Kind::make(cond),
                    step: Kind::empty(),
                    body: Kind::make(body),
                }
            }

            Token::Do => {
                tokens.advance();
                let body = self.statement(tokens);

                expect_token(tokens, Token::While);
                expect_token(tokens, '(');
                let cond = self.assign(tokens);
                expect_token(tokens, ')');
                expect_token(tokens, ';');

                Node::DoWhile {
                    body: Kind::make(body),
                    cond: Kind::make(cond),
                }
            }

            Token::Return => {
                tokens.advance();
                let node = Node::Return {
                    expr: Kind::make(self.assign(tokens)),
                };
                expect_token(tokens, ';');
                node
            }

            tok if *tok == '{' => {
                tokens.advance();
                let mut stmts = vec![];
                while !consume(tokens, '}') {
                    stmts.push(Kind::make(self.statement(tokens)));
                }
                Node::Compound { stmts }
            }
            tok if *tok == ';' => {
                tokens.advance();
                Node::Noop {}
            }
            tok if *tok != Token::EOF => self.expression(tokens),
            _ => {
                expect_fail("", pos, "token wasn't") // expected
            }
        }
    }

    fn expression(&mut self, tokens: &mut Tokens) -> Node {
        let node = Node::Expression {
            expr: Kind::make(self.assign(tokens)),
        };
        expect_token(tokens, ';');
        node
    }

    fn declaration(&mut self, tokens: &mut Tokens) -> Node {
        let ty = self.type_(tokens);

        let size = types::size_of(&ty);
        let name = self.ident(tokens);
        let array = self.read_array(tokens, ty);

        let init = if consume(tokens, '=') {
            Kind::make(self.assign(tokens))
        } else {
            Kind::empty()
        };

        expect_token(tokens, ';');
        Node::Vardef {
            name,
            init,
            offset: 0,
            ty: Rc::new(RefCell::new(array)),
            data: size,
            is_extern: false,
        }
    }

    fn assign(&mut self, tokens: &mut Tokens) -> Node {
        let lhs = self.logor(tokens);
        if consume(tokens, '=') {
            return Node::Assign {
                lhs: Kind::make(lhs),
                rhs: Kind::make(self.logor(tokens)),
            };
        }
        lhs
    }

    fn logor(&mut self, tokens: &mut Tokens) -> Node {
        let mut lhs = self.logand(tokens);
        'expr: loop {
            if let Some((_, Token::MChar('|', '|'))) = tokens.peek() {
                tokens.advance();
                lhs = Node::LogOr {
                    lhs: Kind::make(lhs),
                    rhs: Kind::make(self.logand(tokens)),
                };
            } else {
                break 'expr;
            }
        }
        lhs
    }

    fn logand(&mut self, tokens: &mut Tokens) -> Node {
        let mut lhs = self.equality(tokens);
        'expr: loop {
            if let Some((_, Token::MChar('&', '&'))) = tokens.peek() {
                tokens.advance();
                lhs = Node::LogAnd {
                    lhs: Kind::make(lhs),
                    rhs: Kind::make(self.equality(tokens)),
                };
            } else {
                break 'expr;
            }
        }
        lhs
    }

    fn equality(&mut self, tokens: &mut Tokens) -> Node {
        let mut lhs = self.relative(tokens);
        'expr: loop {
            let (_, next) = tokens.peek().expect("token for rel");
            match next {
                Token::MChar('=', '=') => {
                    tokens.advance();
                    lhs = Node::Equals {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(self.relative(tokens)),
                    };
                }
                Token::MChar('!', '=') => {
                    tokens.advance();
                    lhs = Node::NEquals {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(self.relative(tokens)),
                    };
                }
                _ => break 'expr,
            }
        }
        lhs
    }

    fn relative(&mut self, tokens: &mut Tokens) -> Node {
        let mut lhs = self.add(tokens);
        'expr: loop {
            let (_, next) = tokens.peek().expect("token for rel");
            match next {
                tok if *tok == '<' => {
                    tokens.advance();
                    lhs = Node::Comparison {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(self.add(tokens)),
                        comp: Comp::Lt,
                    };
                }
                tok if *tok == '>' => {
                    tokens.advance();
                    lhs = Node::Comparison {
                        lhs: Kind::make(self.add(tokens)),
                        rhs: Kind::make(lhs),
                        comp: Comp::Gt,
                    };
                }
                _ => break 'expr,
            }
        }
        lhs
    }

    fn add(&mut self, tokens: &mut Tokens) -> Node {
        let mut lhs = self.multiply(tokens);
        'expr: loop {
            let (_, next) = tokens.peek().expect("token for add");
            match next {
                tok if *tok == '+' => {
                    tokens.advance();
                    lhs = Node::Add {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(self.multiply(tokens)),
                    };
                }
                tok if *tok == '-' => {
                    tokens.advance();
                    lhs = Node::Sub {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(self.multiply(tokens)),
                    };
                }
                _ => break 'expr,
            }
        }
        lhs
    }

    fn multiply(&mut self, tokens: &mut Tokens) -> Node {
        let mut lhs = self.unary(tokens);
        'expr: loop {
            let (_, next) = tokens.peek().expect("token for mul");
            match next {
                tok if *tok == '*' => {
                    tokens.advance();
                    lhs = Node::Mul {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(self.unary(tokens)),
                    };
                }
                tok if *tok == '/' => {
                    tokens.advance();
                    lhs = Node::Div {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(self.unary(tokens)),
                    };
                }
                _ => break 'expr,
            }
        }
        lhs
    }

    fn unary(&mut self, tokens: &mut Tokens) -> Node {
        if consume(tokens, '*') {
            return Node::Deref {
                expr: Kind::make(self.multiply(tokens)),
            };
        }
        if consume(tokens, '&') {
            return Node::Addr {
                expr: Kind::make(self.multiply(tokens)),
                ty: Rc::new(RefCell::new(Type::Int)), // TODO: ?? what to do here
            };
        }
        if consume(tokens, Token::Sizeof) {
            return Node::Sizeof {
                expr: Kind::make(self.unary(tokens)),
            };
        }
        if consume(tokens, Token::Alignof) {
            return Node::Alignof {
                expr: Kind::make(self.unary(tokens)),
            };
        }
        self.postfix(tokens)
    }

    fn primary(&mut self, tokens: &mut Tokens) -> Node {
        let (pos, next) = tokens.next_token().expect("token for term");
        match next {
            tok if *tok == '(' => {
                if consume(tokens, '{') {
                    let stmt = self.compound(tokens);
                    expect_token(tokens, ')');
                    return Node::Statement {
                        stmt: Kind::make(stmt),
                        ty: Rc::new(RefCell::new(Type::Int)),
                    };
                }
                let node = self.assign(tokens);
                expect_token(tokens, ')');
                node
            }

            Token::Num(n) => Node::Constant {
                val: *n,
                ty: Rc::new(RefCell::new(Type::Int)),
            },

            Token::Str(s) => Node::Str {
                str: s.to_string(),
                ty: Rc::new(RefCell::new(types::array_of(
                    Rc::new(RefCell::new(Type::Char)),
                    s.len(),
                ))),
            },

            Token::Ident(ref name) => {
                let name = name.to_string();
                if !consume(tokens, '(') {
                    return Node::Ident { name };
                }

                if consume(tokens, ')') {
                    return Node::Call { name, args: vec![] };
                }

                let mut args = vec![];
                args.push(Kind::make(self.assign(tokens)));
                while consume(tokens, ',') {
                    args.push(Kind::make(self.assign(tokens)));
                }
                expect_token(tokens, ')');
                Node::Call { name, args }
            }

            _ => expect_fail("", pos, "number or ident"),
        }
    }

    fn postfix(&mut self, tokens: &mut Tokens) -> Node {
        let mut lhs = self.primary(tokens);
        loop {
            if consume(tokens, '.') {
                lhs = Node::Dot {
                    offset: 0,
                    expr: Kind::make(lhs),
                    name: self.ident(tokens),
                };
                continue;
            }

            if consume(tokens, "->") {
                lhs = Node::Dot {
                    offset: 0,
                    expr: Kind::make(Node::Deref {
                        expr: Kind::make(lhs),
                    }),
                    name: self.ident(tokens),
                };
                continue;
            }

            if consume(tokens, '[') {
                lhs = Node::Deref {
                    expr: Kind::make(Node::Add {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(self.assign(tokens)),
                    }),
                };
                expect_token(tokens, ']');
                continue;
            }
            return lhs;
        }
    }

    fn read_array(&mut self, tokens: &mut Tokens, ty: Type) -> Type {
        let mut param = vec![];
        while consume(tokens, '[') {
            match self.primary(tokens) {
                Node::Constant { val, .. } => param.push(val),
                _ => expect_fail("", &tokens.pos(), "number"),
            };
            expect_token(tokens, ']');
        }

        let mut ty = ty;
        for el in param.iter().rev() {
            ty = types::array_of(Rc::new(RefCell::new(ty)), *el as usize);
        }
        ty
    }

    fn ident(&mut self, tokens: &mut Tokens) -> String {
        if let (_, Token::Ident(name)) = expect(tokens, Token::Ident("".into()), "identifier") {
            return name;
        }
        unreachable!()
    }

    fn type_(&mut self, tokens: &mut Tokens) -> Type {
        let (_pos, token) = expect_tokens(
            tokens,
            &[
                Token::Type(TokType::Char), // are you serious
                Token::Type(TokType::Int),
                Token::Struct,
            ],
        );

        let mut ty = match token {
            Token::Type(ty) => match ty {
                TokType::Char => Type::Char,
                TokType::Int => Type::Int,
            },
            Token::Struct => {
                let tag = if let Some((_, Token::Ident(s))) = tokens.peek() {
                    let s = Some(s.clone());
                    tokens.advance();
                    s
                } else {
                    None
                };

                let mut members = if consume(tokens, '{') {
                    let mut v = vec![];
                    while !consume(tokens, '}') {
                        v.push(self.declaration(tokens));
                    }
                    Some(v)
                } else {
                    None
                };

                match (tag.is_none(), members.is_none()) {
                    (true, true) => {
                        fail!("bad struct definition");
                    }
                    (false, false) => {
                        self.env.tags.insert(
                            tag.as_ref().unwrap().to_string(),
                            members.as_ref().unwrap().clone(),
                        );
                    }
                    (false, true) => {
                        let tag = tag.unwrap();
                        members = self
                            .env
                            .tags
                            .get(tag.as_str())
                            .and_then(|m| Some(m.clone()));
                        if members.is_none() {
                            fail!("incomplete type: {}", tag);
                        }
                    }
                    _ => {} // no tag
                };

                types::struct_of(&members.as_ref().unwrap())
            }
            _ => unreachable!(),
        };

        while consume(tokens, '*') {
            ty = types::ptr_of(Rc::new(RefCell::new(ty)));
        }
        ty
    }
}

#[inline]
fn is_typename(tokens: &mut Tokens) -> bool {
    match tokens.peek() {
        Some((_, Token::Type(_))) | Some((_, Token::Struct)) => true,
        _ => false,
    }
}

#[inline]
fn consume(tokens: &mut Tokens, tok: impl Into<Token>) -> bool {
    let tok = tok.into();
    match tokens.peek() {
        Some((_, t)) if *t == tok => {
            tokens.advance();
            true
        }
        _ => false,
    }
}

#[inline]
fn expect_tokens<'a>(tokens: &'a mut Tokens, toks: &[Token]) -> (Span<'a>, Token) {
    let (pos, next) = tokens.next_token().expect("get next token");

    for tok in toks {
        if *next == *tok {
            return (*pos, next.clone());
        }
    }

    const SOURCE_LINE: &str = "source=> ";
    let (pos, next) = (*pos, next.clone());
    // TODO fix this
    let (input, adjusted) = midpoint("", 0, 80 - SOURCE_LINE.len());

    eprintln!("{}{}", wrap_color!(Color::Yellow {}, SOURCE_LINE), input);
    draw_caret(SOURCE_LINE.len() + adjusted, Color::Red {});

    let mut out = String::new();
    for (i, s) in toks
        .iter()
        .map(|t| match t {
            Token::Char(c) => c.to_string(),
            tok => format!("{}", tok),
        }).enumerate()
    {
        out.push_str(&s);
        if i < toks.len() {
            out.push_str(", ");
        }
    }

    fail!(
        "{} one of {} was expected. found {} at position: {}.\n",
        wrap_color!(Color::Red {}, "ERROR:"),
        wrap_color!(Color::Green {}, out),
        wrap_color!(Color::Cyan {}, "{:?}", next),
        wrap_color!(Color::Blue {}, pos),
    );
}

#[inline]
fn expect_token(tokens: &mut Tokens, tok: impl Into<Token>) {
    let tok = tok.into();

    let (pos, next) = tokens.next_token().expect("get next token");
    if *next == tok {
        return;
    }

    const SOURCE_LINE: &str = "source=> ";
    let (pos, next) = (*pos, next.clone());
    // TODO fix this
    let (input, adjusted) = midpoint("", 0, 80 - SOURCE_LINE.len());

    eprintln!("{}{}", wrap_color!(Color::Yellow {}, SOURCE_LINE), input);
    draw_caret(SOURCE_LINE.len() + adjusted, Color::Red {});

    fail!(
        "{} {} was expected. found {} at position: {}.\n",
        wrap_color!(Color::Red {}, "ERROR:"),
        wrap_color!(Color::Green {}, tok.get_char()),
        wrap_color!(Color::Cyan {}, "{}", next),
        wrap_color!(Color::Blue {}, pos),
    );
}

/// this uses a discriminant comparison
#[inline]
#[allow(dead_code)]
fn expect<'a>(
    tokens: &'a mut Tokens,
    tok: impl Into<Token>,
    msg: impl AsRef<str>,
) -> (Span<'a>, Token) {
    use std::mem::discriminant;
    let (pos, next) = tokens.next_token().expect("get next token");
    if discriminant(next) == discriminant(&tok.into()) {
        return (*pos, next.clone());
    }
    expect_fail("", pos, msg.as_ref());
}

#[inline]
fn expect_type<'a>(tokens: &'a mut Tokens, msg: impl AsRef<str>) -> (Span<'a>, TokType) {
    let (pos, next) = tokens.next_token().expect("get next token");
    if let Token::Type(ty) = next {
        return (*pos, ty.clone());
    }
    expect_fail("", pos, msg.as_ref());
}

#[inline]
fn expect_ident<'a>(tokens: &'a mut Tokens, msg: impl AsRef<str>) -> (Span<'a>, String) {
    let (pos, next) = tokens.next_token().expect("get next token");
    if let Token::Ident(name) = next {
        return (*pos, name.to_string());
    }
    expect_fail("", pos, msg.as_ref());
}

// lifetimes ??
fn expect_fail<'a>(input: &str, span: &'a Span<'a>, msg: &str) -> ! {
    const SOURCE_LINE: &str = "source=> ";
    // TODO: fix this
    let (input, adjusted) = midpoint(&input, 0, 80 - SOURCE_LINE.len());

    eprintln!("{}{}", wrap_color!(Color::Yellow {}, SOURCE_LINE), input);
    draw_caret(SOURCE_LINE.len() + adjusted, Color::Red {});

    fail!(
        "{} {} was expected. at position: {}.\n",
        wrap_color!(Color::Red {}, "ERROR:"),
        wrap_color!(Color::Cyan {}, msg),
        wrap_color!(Color::Blue {}, span),
    );
}
