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
        ty: Option<Type>,
    },

    Sub {
        lhs: Kind,
        rhs: Kind,
        ty: Option<Type>,
    },

    Mul {
        lhs: Kind,
        rhs: Kind,
        ty: Option<Type>,
    },

    Div {
        lhs: Kind,
        rhs: Kind,
        ty: Option<Type>,
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
            Node::Add { ty, .. }
            | Node::Sub { ty, .. }
            | Node::Mul { ty, .. }
            | Node::Div { ty, .. } => ty.as_ref().expect("get type"),

            Node::Addr { ty, .. } => ty,
            Node::Deref { expr } => expr.get_type(),

            // Node::Return { expr } => expr.get_type(),
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
            Node::Add { ty, .. }
            | Node::Sub { ty, .. }
            | Node::Mul { ty, .. }
            | Node::Div { ty, .. } => {
                ty.get_or_insert(newtype);
            }

            Node::Deref { expr, .. } => expr.set_type(newtype),

            Node::Assign { lhs, .. } => lhs.set_type(newtype),

            // this must panic .. WHY?
            _ => panic!("can't set type"),
        };
    }
}

impl AsMut<Node> for Node {
    fn as_mut(&mut self) -> &mut Node {
        self
    }
}

impl Node {
    pub fn parse(mut tokens: Tokens) -> Vec<Self> {
        let tokens = &mut tokens;

        let mut nodes = vec![];
        while let Some((_, tok)) = tokens.peek() {
            if *tok == Token::EOF {
                break;
            }
            nodes.push(Self::top_level(tokens))
        }
        nodes
    }

    fn top_level(tokens: &mut Tokens) -> Self {
        let is_extern = consume(tokens, Token::Extern);

        let mut ty = Self::ty(tokens);
        let (_, name) = expect_ident(tokens, "function name");

        // functions
        if consume(tokens, '(') {
            let mut args = vec![];
            if !consume(tokens, ')') {
                args.push(Kind::make(Self::param(tokens)));
                while consume(tokens, ',') {
                    args.push(Kind::make(Self::param(tokens)));
                }
                expect_token(tokens, ')');
            }
            expect_token(tokens, '{');

            return Node::Func {
                ty: ty.clone(),
                body: Kind::make(Self::compound_stmt(tokens)),
                name: name.clone(),
                args,
                stacksize: 0,
                globals: vec![],
            };
        }

        let data = if is_extern { 0 } else { ty.size_of() }; // -1 for no data

        let node = Node::Vardef {
            ty: Self::read_array(tokens, &mut ty).clone(),
            name: name.clone(),
            init: Kind::empty(),
            offset: 0,
            data,
            is_extern,
        };

        expect_token(tokens, ';');
        node
    }

    fn param(tokens: &mut Tokens) -> Self {
        let ty = Self::ty(tokens);

        let (_, name) = expect_ident(tokens, "variable name as param");
        let name = name.to_string();

        let init = if consume(tokens, '=') {
            Kind::make(Self::assign(tokens))
        } else {
            Kind::empty()
        };

        let data = ty.size_of();
        Node::Vardef {
            name,
            init,
            offset: 0,
            ty,
            data,
            is_extern: false,
        }
    }

    fn compound_stmt(tokens: &mut Tokens) -> Self {
        let mut stmts = vec![];
        while !consume(tokens, '}') {
            stmts.push(Kind::make(Self::stmt(tokens)))
        }
        Node::Compound { stmts }
    }

    fn stmt(tokens: &mut Tokens) -> Self {
        let (pos, next) = tokens.peek().expect("token for statement");

        match next {
            Token::Type(_) => Self::decl(tokens),
            Token::If => {
                tokens.advance();
                expect_token(tokens, '(');
                let cond = Kind::make(Self::assign(tokens));

                expect_token(tokens, ')');
                let body = Kind::make(Self::stmt(tokens));

                let else_ = if consume(tokens, "else") {
                    Kind::make(Self::stmt(tokens))
                } else {
                    Kind::empty()
                };

                Node::If { cond, body, else_ }
            }
            Token::For => {
                tokens.advance();
                expect_token(tokens, '(');
                let init = if is_typename(tokens) {
                    Self::decl(tokens)
                } else {
                    Self::expr_stmt(tokens)
                };

                let cond = Self::assign(tokens);
                expect_token(tokens, ';');

                let step = Node::Expression {
                    expr: Kind::make(Self::assign(tokens)),
                };

                expect_token(tokens, ')');

                let body = Self::stmt(tokens);
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
                let cond = Self::assign(tokens);

                expect_token(tokens, ')');
                let body = Self::stmt(tokens);

                Node::For {
                    init: Kind::empty(),
                    cond: Kind::make(cond),
                    step: Kind::empty(),
                    body: Kind::make(body),
                }
            }

            Token::Do => {
                tokens.advance();
                let body = Self::stmt(tokens);

                expect_token(tokens, Token::While);
                expect_token(tokens, '(');
                let cond = Self::assign(tokens);
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
                    expr: Kind::make(Node::assign(tokens)),
                };
                expect_token(tokens, ';');
                node
            }
            tok if *tok == '{' => {
                tokens.advance();
                let mut stmts = vec![];
                while !consume(tokens, '}') {
                    stmts.push(Kind::make(Self::stmt(tokens)));
                }
                Node::Compound { stmts }
            }
            tok if *tok == ';' => {
                tokens.advance();
                Node::Noop {}
            }
            tok if *tok != Token::EOF => Self::expr_stmt(tokens),
            _ => {
                expect_fail("", pos, "token wasn't") // expected
            }
        }
    }

    fn expr_stmt(tokens: &mut Tokens) -> Self {
        let node = Node::Expression {
            expr: Kind::make(Self::assign(tokens)),
        };
        expect_token(tokens, ';');
        node
    }

    fn decl(tokens: &mut Tokens) -> Self {
        let mut ty = Self::ty(tokens);

        let (_, name) = expect_ident(tokens, "variable name as decl");
        let name = name.to_string();

        let array = Self::read_array(tokens, &mut ty);

        let init = if consume(tokens, '=') {
            Kind::make(Self::assign(tokens))
        } else {
            Kind::empty()
        };

        expect_token(tokens, ';');
        Node::Vardef {
            name,
            init,
            offset: 0,
            ty: array.clone(),
            data: ty.size_of(),
            is_extern: false,
        }
    }

    fn assign(tokens: &mut Tokens) -> Self {
        let lhs = Self::logor(tokens);
        if consume(tokens, '=') {
            return Node::Assign {
                lhs: Kind::make(lhs),
                rhs: Kind::make(Self::logor(tokens)),
            };
        }
        lhs
    }

    fn logor(tokens: &mut Tokens) -> Self {
        let mut lhs = Self::logand(tokens);
        'expr: loop {
            if let Some((_, Token::MChar('|', '|'))) = tokens.peek() {
                tokens.advance();
                lhs = Node::LogOr {
                    lhs: Kind::make(lhs),
                    rhs: Kind::make(Self::logand(tokens)),
                };
            } else {
                break 'expr;
            }
        }
        lhs
    }

    fn logand(tokens: &mut Tokens) -> Self {
        let mut lhs = Self::equality(tokens);
        'expr: loop {
            if let Some((_, Token::MChar('&', '&'))) = tokens.peek() {
                tokens.advance();
                lhs = Node::LogAnd {
                    lhs: Kind::make(lhs),
                    rhs: Kind::make(Self::equality(tokens)),
                };
            } else {
                break 'expr;
            }
        }
        lhs
    }

    fn equality(tokens: &mut Tokens) -> Self {
        let mut lhs = Self::rel(tokens);
        'expr: loop {
            let (_, next) = tokens.peek().expect("token for rel");
            match next {
                Token::MChar('=', '=') => {
                    tokens.advance();
                    lhs = Node::Equals {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(Self::rel(tokens)),
                    };
                }
                Token::MChar('!', '=') => {
                    tokens.advance();
                    lhs = Node::NEquals {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(Self::rel(tokens)),
                    };
                }
                _ => break 'expr,
            }
        }
        lhs
    }

    fn rel(tokens: &mut Tokens) -> Self {
        let mut lhs = Self::add(tokens);
        'expr: loop {
            let (_, next) = tokens.peek().expect("token for rel");
            match next {
                tok if *tok == '<' => {
                    tokens.advance();
                    lhs = Node::Comparison {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(Self::add(tokens)),
                        comp: Comp::Lt,
                    };
                }
                tok if *tok == '>' => {
                    tokens.advance();
                    lhs = Node::Comparison {
                        lhs: Kind::make(Self::add(tokens)),
                        rhs: Kind::make(lhs),
                        comp: Comp::Gt,
                    };
                }
                _ => break 'expr,
            }
        }
        lhs
    }

    fn add(tokens: &mut Tokens) -> Self {
        let mut lhs = Self::mul(tokens);
        'expr: loop {
            let (_, next) = tokens.peek().expect("token for add");
            match next {
                tok if *tok == '+' => {
                    tokens.advance();
                    lhs = Node::Add {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(Self::mul(tokens)),
                        ty: None,
                    };
                }
                tok if *tok == '-' => {
                    tokens.advance();
                    lhs = Node::Sub {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(Self::mul(tokens)),
                        ty: None,
                    };
                }
                _ => break 'expr,
            }
        }
        lhs
    }

    fn mul(tokens: &mut Tokens) -> Self {
        let mut lhs = Self::unary(tokens);
        'expr: loop {
            let (_, next) = tokens.peek().expect("token for mul");
            match next {
                tok if *tok == '*' => {
                    tokens.advance();
                    lhs = Node::Mul {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(Self::unary(tokens)),
                        ty: None,
                    };
                }
                tok if *tok == '/' => {
                    tokens.advance();
                    lhs = Node::Div {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(Self::unary(tokens)),
                        ty: None,
                    };
                }
                _ => break 'expr,
            }
        }
        lhs
    }

    fn unary(tokens: &mut Tokens) -> Self {
        if consume(tokens, '*') {
            return Node::Deref {
                expr: Kind::make(Self::mul(tokens)),
            };
        }
        if consume(tokens, '&') {
            return Node::Addr {
                expr: Kind::make(Self::mul(tokens)),
                ty: Type::Int, // ?? what to do here
            };
        }
        if consume(tokens, Token::Sizeof) {
            return Node::Sizeof {
                expr: Kind::make(Self::unary(tokens)),
            };
        }
        Self::postfix(tokens)
    }

    fn primary(tokens: &mut Tokens) -> Self {
        let (pos, next) = tokens.next_token().expect("token for term");
        match next {
            tok if *tok == '(' => {
                if consume(tokens, '{') {
                    let stmt = Self::compound_stmt(tokens);
                    expect_token(tokens, ')');
                    return Node::Statement {
                        stmt: Kind::make(stmt),
                        ty: Type::Int,
                    };
                }
                let node = Node::assign(tokens);
                expect_token(tokens, ')');
                node
            }

            Token::Num(n) => Node::Constant {
                val: *n,
                ty: Type::Int,
            },

            Token::Str(ref s) => Node::Str {
                str: s.clone(),
                ty: Type::Array {
                    base: Box::new(Type::Char),
                    len: s.len(),
                    data: vec![], // HACK: probably not needed
                },
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
                args.push(Kind::make(Self::assign(tokens)));
                while consume(tokens, ',') {
                    args.push(Kind::make(Self::assign(tokens)));
                }
                expect_token(tokens, ')');
                Node::Call { name, args }
            }

            _ => expect_fail("", pos, "number or ident"),
        }
    }

    fn postfix(tokens: &mut Tokens) -> Self {
        let mut lhs = Self::primary(tokens);
        while consume(tokens, '[') {
            lhs = Node::Deref {
                expr: Kind::make(Node::Add {
                    lhs: Kind::make(lhs),
                    rhs: Kind::make(Self::assign(tokens)),
                    ty: None,
                }),
            };
            expect_token(tokens, ']')
        }
        lhs
    }

    fn read_array<'a>(tokens: &mut Tokens, ty: &'a mut Type) -> &'a Type {
        let mut param = vec![];
        while consume(tokens, '[') {
            match Self::primary(tokens) {
                Node::Constant { val, .. } => param.push(val),
                _ => expect_fail("", &tokens.pos(), "number"),
            };
            expect_token(tokens, ']');
        }

        for el in param.iter().rev() {
            *ty = Type::Array {
                base: Box::new(ty.clone()),
                len: *el as usize,
                data: vec![],
            }
        }

        ty
    }

    fn ty(tokens: &mut Tokens) -> Type {
        let (_pos, ty) = expect_type(tokens, "typename");

        let mut ty = match ty {
            tokens::Type::Char => Type::Char,
            tokens::Type::Int => Type::Int,
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
        Some((_, Token::Type(_))) => true,
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

// TODO track col:row + filename
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
        wrap_color!(Color::Cyan {}, "{:?}", next),
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
