#![allow(dead_code)]
use super::*;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Constant {
        val: u32, // XXX: why is this a u32
        ty: Type,
    },

    Ident {
        name: String,
    },

    Str {
        str: String,
        ty: Type, // array
    },

    Addr {
        expr: Kind,
        ty: Type,
    },

    Add {
        lhs: Kind,
        rhs: Kind,
    },

    Sub {
        lhs: Kind,
        rhs: Kind,
    },

    Mul {
        lhs: Kind,
        rhs: Kind,
    },

    Div {
        lhs: Kind,
        rhs: Kind,
    },

    LogAnd {
        lhs: Kind,
        rhs: Kind,
    },

    LogOr {
        lhs: Kind,
        rhs: Kind,
    },

    Equals {
        lhs: Kind,
        rhs: Kind,
    },

    NEquals {
        lhs: Kind,
        rhs: Kind,
    },

    Assign {
        lhs: Kind,
        rhs: Kind,
    },

    Comparison {
        lhs: Kind,
        rhs: Kind,
        comp: Comp,
    },

    LVal {
        offset: i32,
        ty: Type,
    },

    GVar {
        name: String,
        ty: Type,
    },

    Deref {
        expr: Kind,
    },

    Vardef {
        ty: Type,
        name: String,
        init: Kind,
        offset: i32,
        data: i32,
        is_extern: bool,
    },

    Return {
        expr: Kind,
    },

    Sizeof {
        expr: Kind,
    },

    Alignof {
        expr: Kind,
    },

    Struct {
        members: Vec<Kind>,
        offset: i32, // used for alignment
    },

    Dot {
        expr: Kind,
        member: String,
        offset: i32, // used for offset into the struct
    },

    If {
        cond: Kind,
        body: Kind,
        else_: Kind,
    },

    DoWhile {
        body: Kind,
        cond: Kind,
    },

    Else {
        body: Kind,
    },

    For {
        init: Kind,
        cond: Kind,
        step: Kind,
        body: Kind,
    },

    Call {
        name: String,
        args: Vec<Kind>,
    },

    Func {
        name: String,
        args: Vec<Kind>,
        body: Kind,
        stacksize: i32,
        ty: Type,
        globals: Vec<Var>,
    },

    Statement {
        stmt: Kind,
        ty: Type,
    },

    Expression {
        expr: Kind,
    },

    Compound {
        stmts: Vec<Kind>,
    },

    Noop {},
}

#[derive(Debug, Clone, PartialEq)]
pub enum Comp {
    Lt,
    Gt,
}

impl fmt::Display for Comp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let c = match self {
            Comp::Lt => '<',
            Comp::Gt => '>',
        };
        write!(f, "{}", c)
    }
}

impl Node {
    /// instrinsic types
    pub(crate) fn has_type(&self) -> bool {
        match self {
            Node::Constant { .. } | Node::GVar { .. } | Node::LVal { .. } | Node::Vardef { .. } => {
                true
            }
            _ => false,
        }
    }

    pub(crate) fn get_type(&self) -> &Type {
        match self {
            Node::Add { lhs, .. }
            | Node::Sub { lhs, .. }
            | Node::Mul { lhs, .. }
            | Node::Div { lhs, .. } => lhs.get_type(),

            Node::Addr { ty, .. } => ty,
            Node::Deref { expr } | Node::Dot { expr, .. } => expr.get_type(),

            Node::Constant { ty, .. }
            | Node::Statement { ty, .. }
            | Node::GVar { ty, .. }
            | Node::LVal { ty, .. }
            | Node::Vardef { ty, .. } => ty,
            _ => fail!("doesn't have a type\n{:#?}", self),
        }
    }

    pub(crate) fn set_type(&mut self, newtype: Type) {
        match self {
            Node::Add { lhs, rhs }
            | Node::Sub { lhs, rhs }
            | Node::Mul { lhs, rhs }
            | Node::Div { lhs, rhs } => {
                lhs.set_type(newtype.clone());
                rhs.set_type(newtype);
            }
            Node::Deref { expr, .. } | Node::Dot { expr, .. } => expr.set_type(newtype),
            Node::Assign { lhs, .. } => lhs.set_type(newtype),
            _ => unreachable!(),
        };
    }
}

impl AsMut<Node> for Node {
    fn as_mut(&mut self) -> &mut Node {
        self
    }
}

pub struct Parser {}

impl Parser {
    pub fn parse(mut tokens: Tokens) -> Vec<Node> {
        let tokens = &mut tokens;
        let mut this = Parser {};
        let mut nodes = vec![];
        while let Some((_, tok)) = tokens.peek() {
            if *tok == Token::EOF {
                break;
            }
            nodes.push(this.top_level(tokens))
        }
        nodes
    }

    fn top_level(&mut self, tokens: &mut Tokens) -> Node {
        let is_extern = consume(tokens, Token::Extern);
        let mut ty = self.ty(tokens);
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
                ty: ty.clone(),
                body: Kind::make(self.compound_stmt(tokens)),
                name,
                args,
                stacksize: 0,
                globals: vec![],
            };
        }

        let data = if is_extern { 0 } else { ty.size() }; // -1 for no data

        let node = Node::Vardef {
            ty: self.read_array(tokens, &mut ty).clone(),
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
        let ty = self.ty(tokens);
        let name = self.ident(tokens);
        let init = if consume(tokens, '=') {
            Kind::make(self.assign(tokens))
        } else {
            Kind::empty()
        };

        let data = ty.size();
        Node::Vardef {
            name,
            init,
            offset: 0,
            ty,
            data,
            is_extern: false,
        }
    }

    fn compound_stmt(&mut self, tokens: &mut Tokens) -> Node {
        let mut stmts = vec![];
        while !consume(tokens, '}') {
            stmts.push(Kind::make(self.stmt(tokens)))
        }
        Node::Compound { stmts }
    }

    fn stmt(&mut self, tokens: &mut Tokens) -> Node {
        let (pos, next) = tokens.peek().expect("token for statement");

        match next {
            Token::Type(_) | Token::Struct => self.decl(tokens),
            Token::If => {
                tokens.advance();
                expect_token(tokens, '(');
                let cond = Kind::make(self.assign(tokens));

                expect_token(tokens, ')');
                let body = Kind::make(self.stmt(tokens));

                let else_ = if consume(tokens, "else") {
                    Kind::make(self.stmt(tokens))
                } else {
                    Kind::empty()
                };

                Node::If { cond, body, else_ }
            }
            Token::For => {
                tokens.advance();
                expect_token(tokens, '(');
                let init = if is_typename(tokens) {
                    self.decl(tokens)
                } else {
                    self.expr_stmt(tokens)
                };

                let cond = self.assign(tokens);
                expect_token(tokens, ';');

                let step = Node::Expression {
                    expr: Kind::make(self.assign(tokens)),
                };

                expect_token(tokens, ')');

                let body = self.stmt(tokens);
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
                let body = self.stmt(tokens);

                Node::For {
                    init: Kind::empty(),
                    cond: Kind::make(cond),
                    step: Kind::empty(),
                    body: Kind::make(body),
                }
            }

            Token::Do => {
                tokens.advance();
                let body = self.stmt(tokens);

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
                    stmts.push(Kind::make(self.stmt(tokens)));
                }
                Node::Compound { stmts }
            }
            tok if *tok == ';' => {
                tokens.advance();
                Node::Noop {}
            }
            tok if *tok != Token::EOF => self.expr_stmt(tokens),
            _ => {
                expect_fail("", pos, "token wasn't") // expected
            }
        }
    }

    fn expr_stmt(&mut self, tokens: &mut Tokens) -> Node {
        let node = Node::Expression {
            expr: Kind::make(self.assign(tokens)),
        };
        expect_token(tokens, ';');
        node
    }

    fn decl(&mut self, tokens: &mut Tokens) -> Node {
        let mut ty = self.ty(tokens);

        let name = self.ident(tokens);
        let array = self.read_array(tokens, &mut ty);
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
            ty: array.clone(),
            data: ty.size(),
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
        let mut lhs = self.rel(tokens);
        'expr: loop {
            let (_, next) = tokens.peek().expect("token for rel");
            match next {
                Token::MChar('=', '=') => {
                    tokens.advance();
                    lhs = Node::Equals {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(self.rel(tokens)),
                    };
                }
                Token::MChar('!', '=') => {
                    tokens.advance();
                    lhs = Node::NEquals {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(self.rel(tokens)),
                    };
                }
                _ => break 'expr,
            }
        }
        lhs
    }

    fn rel(&mut self, tokens: &mut Tokens) -> Node {
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
        let mut lhs = self.mul(tokens);
        'expr: loop {
            let (_, next) = tokens.peek().expect("token for add");
            match next {
                tok if *tok == '+' => {
                    tokens.advance();
                    lhs = Node::Add {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(self.mul(tokens)),
                    };
                }
                tok if *tok == '-' => {
                    tokens.advance();
                    lhs = Node::Sub {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(self.mul(tokens)),
                    };
                }
                _ => break 'expr,
            }
        }
        lhs
    }

    fn mul(&mut self, tokens: &mut Tokens) -> Node {
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
                expr: Kind::make(self.mul(tokens)),
            };
        }
        if consume(tokens, '&') {
            return Node::Addr {
                expr: Kind::make(self.mul(tokens)),
                ty: Type::Int, // ?? what to do here
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
                    let stmt = self.compound_stmt(tokens);
                    expect_token(tokens, ')');
                    return Node::Statement {
                        stmt: Kind::make(stmt),
                        ty: Type::Int,
                    };
                }
                let node = self.assign(tokens);
                expect_token(tokens, ')');
                node
            }

            Token::Num(n) => Node::Constant {
                val: *n,
                ty: Type::Int,
            },

            Token::Str(s) => Node::Str {
                str: s.clone(),
                ty: Type::array_of(&Type::Char, s.len()),
            },

            Token::Ident(ref name) => {
                let name = name.clone();
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

    fn ident(&mut self, tokens: &mut Tokens) -> String {
        if let (_, Token::Ident(name)) = expect(tokens, Token::Ident("".into()), "identifier") {
            return name;
        }
        unreachable!()
    }

    fn postfix(&mut self, tokens: &mut Tokens) -> Node {
        let mut lhs = self.primary(tokens);
        if consume(tokens, '.') {
            return Node::Dot {
                offset: 0,
                expr: Kind::make(lhs),
                member: self.ident(tokens),
            };
        }

        if consume(tokens, "->") {
            return Node::Dot {
                offset: 0,
                expr: Kind::make(Node::Deref {
                    expr: Kind::make(lhs),
                }),
                member: self.ident(tokens),
            };
        }

        while consume(tokens, '[') {
            lhs = Node::Deref {
                expr: Kind::make(Node::Add {
                    lhs: Kind::make(lhs),
                    rhs: Kind::make(self.assign(tokens)),
                }),
            };
            expect_token(tokens, ']')
        }
        lhs
    }

    fn read_array<'a>(&mut self, tokens: &mut Tokens, ty: &'a mut Type) -> &'a Type {
        let mut param = vec![];
        while consume(tokens, '[') {
            match self.primary(tokens) {
                Node::Constant { val, .. } => param.push(val),
                _ => expect_fail("", &tokens.pos(), "number"),
            };
            expect_token(tokens, ']');
        }

        for el in param.iter().rev() {
            *ty = Type::array_of(&ty, *el as usize);
        }

        ty
    }

    fn ty(&mut self, tokens: &mut Tokens) -> Type {
        let (_pos, token) = expect_tokens(
            tokens,
            &[
                Token::Type(tokens::Type::Char), // are you serious
                Token::Type(tokens::Type::Int),
                Token::Struct,
            ],
        );

        let mut ty = match token {
            Token::Type(ty) => match ty {
                tokens::Type::Char => Type::Char,
                tokens::Type::Int => Type::Int,
            },
            Token::Struct => {
                expect_token(tokens, '{');
                let mut members = vec![];
                while !consume(tokens, '}') {
                    members.push(self.decl(tokens))
                }
                Type::struct_of(&members)
            }
            _ => unreachable!(),
        };

        while consume(tokens, '*') {
            ty = ty.ptr_of();
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
fn expect_type<'a>(tokens: &'a mut Tokens, msg: impl AsRef<str>) -> (Span<'a>, tokens::Type) {
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
