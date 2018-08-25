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
        name: String,
        init: Kind,
        offset: i32,
        ty: Type,
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
        strings: Vec<(String, String)>,
    },

    Statement {
        expr: Kind,
    },

    Compound {
        stmts: Vec<Kind>,
    },
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
            Node::Constant { .. } | Node::LVal { .. } | Node::Vardef { .. } => true,
            _ => false,
        }
    }

    pub(crate) fn get_type(&self) -> &Type {
        match self {
            Node::Add { ty, .. }
            | Node::Sub { ty, .. }
            | Node::Mul { ty, .. }
            | Node::Div { ty, .. } => ty.as_ref().expect("type"),

            Node::Addr { ty, .. } => ty,
            Node::Deref { expr } => expr.get_type(),

            Node::Constant { ty, .. } | Node::LVal { ty, .. } | Node::Vardef { ty, .. } => ty,
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

            Node::Deref { expr, .. } => {
                expr.set_type(newtype);
            }

            Node::Assign { lhs, .. } => {
                lhs.set_type(newtype);
            }

            // this must panic .. WHY?
            _ => panic!("can't set type"),
        };
    }
}

impl AsRef<Node> for Kind {
    fn as_ref(&self) -> &Node {
        self.val.as_ref().unwrap()
    }
}

impl AsMut<Node> for Kind {
    fn as_mut(&mut self) -> &mut Node {
        self.val.as_mut().unwrap()
    }
}

impl AsMut<Node> for Node {
    fn as_mut(&mut self) -> &mut Node {
        self
    }
}

impl Node {
    pub fn parse(mut tokens: Lexer) -> Vec<Self> {
        let tokens = &mut tokens;
        let mut nodes = vec![];
        while let Some((_, tok)) = tokens.peek() {
            if *tok == Token::EOF {
                break;
            }
            nodes.push(Self::function(tokens))
        }
        nodes
    }

    fn function(tokens: &mut Lexer) -> Self {
        let (_, _ty) = expect_type(tokens, "function return type");
        let (_, name) = expect_ident(tokens, "function name");

        expect_token(tokens, '(');
        let mut args = vec![];
        if !consume(tokens, ')') {
            args.push(Kind::make(Self::param(tokens)));
            while consume(tokens, ',') {
                args.push(Kind::make(Self::param(tokens)));
            }
            expect_token(tokens, ')');
        }

        expect_token(tokens, '{');
        Node::Func {
            name,
            args,
            body: Kind::make(Self::compound_stmt(tokens)),
            stacksize: 0,
            strings: vec![],
        }
    }

    fn param(tokens: &mut Lexer) -> Self {
        let ty = Self::ty(tokens);

        let (_, name) = expect_ident(tokens, "variable name as param");
        let name = name.to_string();

        let init = if consume(tokens, '=') {
            Kind::make(Self::assign(tokens))
        } else {
            Kind::empty()
        };

        Node::Vardef {
            name,
            init,
            offset: 0,
            ty,
        }
    }

    fn compound_stmt(tokens: &mut Lexer) -> Self {
        let mut stmts = vec![];
        while !consume(tokens, '}') {
            stmts.push(Kind::make(Self::stmt(tokens)))
        }
        Node::Compound { stmts }
    }

    fn stmt(tokens: &mut Lexer) -> Self {
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

                let step = Self::assign(tokens);
                expect_token(tokens, ')');

                let body = Self::stmt(tokens);
                Node::For {
                    init: Kind::make(init),
                    cond: Kind::make(cond),
                    step: Kind::make(step),
                    body: Kind::make(body),
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
            tok if *tok != Token::EOF => Self::expr_stmt(tokens),
            _ => expect_fail(tokens.input_at(0), *pos, "token wasn't"), // expected
        }
    }

    fn expr_stmt(tokens: &mut Lexer) -> Self {
        let node = Node::Statement {
            expr: Kind::make(Self::assign(tokens)),
        };
        expect_token(tokens, ';');
        node
    }

    fn decl(tokens: &mut Lexer) -> Self {
        let mut ty = Self::ty(tokens);

        let (_, name) = expect_ident(tokens, "variable name as decl");
        let name = name.to_string();

        let mut param = vec![];
        while consume(tokens, '[') {
            match Self::primary(tokens) {
                Node::Constant { val, .. } => param.push(val),
                _ => expect_fail(tokens.input_at(0), tokens.pos(), "number"),
            };
            expect_token(tokens, ']');
        }

        for el in param.iter().rev() {
            ty = Type::Array {
                base: Box::new(ty.clone()),
                len: *el as usize,
                data: vec![],
            }
        }

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
            ty,
        }
    }

    fn assign(tokens: &mut Lexer) -> Self {
        let lhs = Self::logor(tokens);
        if consume(tokens, '=') {
            return Node::Assign {
                lhs: Kind::make(lhs),
                rhs: Kind::make(Self::logor(tokens)),
            };
        }
        lhs
    }

    fn logor(tokens: &mut Lexer) -> Self {
        let mut lhs = Self::logand(tokens);
        'expr: loop {
            if let Some((_, Token::LogOr)) = tokens.peek() {
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

    fn logand(tokens: &mut Lexer) -> Self {
        let mut lhs = Self::rel(tokens);
        'expr: loop {
            if let Some((_, Token::LogAnd)) = tokens.peek() {
                tokens.advance();
                lhs = Node::LogAnd {
                    lhs: Kind::make(lhs),
                    rhs: Kind::make(Self::rel(tokens)),
                };
            } else {
                break 'expr;
            }
        }
        lhs
    }

    fn rel(tokens: &mut Lexer) -> Self {
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

    fn add(tokens: &mut Lexer) -> Self {
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

    fn mul(tokens: &mut Lexer) -> Self {
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

    fn unary(tokens: &mut Lexer) -> Self {
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

    fn primary(tokens: &mut Lexer) -> Self {
        let (pos, next) = tokens.next_token().expect("token for term");
        match next {
            tok if *tok == '(' => {
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

            _ => {
                let pos = *pos;
                expect_fail(tokens.input_at(0), pos, "number or ident")
            }
        }
    }

    fn postfix(tokens: &mut Lexer) -> Self {
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

    fn ty(tokens: &mut Lexer) -> Type {
        let (_pos, ty) = expect_type(tokens, "typename");

        let mut ty = match ty {
            LexType::Char => Type::Char,
            LexType::Int => Type::Int,
        };

        while consume(tokens, '*') {
            ty = ty.ptr_of();
        }
        ty
    }
}

#[inline]
fn is_typename(tokens: &mut Lexer) -> bool {
    match tokens.peek() {
        Some((_, Token::Type(_))) => true,
        _ => false,
    }
}

#[inline]
fn consume(tokens: &mut Lexer, tok: impl Into<Token>) -> bool {
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
fn expect_token(tokens: &mut Lexer, tok: impl Into<Token>) {
    let tok = tok.into();

    let (pos, next) = tokens.next_token().expect("get next token");
    if *next == tok {
        return;
    }

    const SOURCE_LINE: &str = "source=> ";
    let (pos, next) = (*pos, next.clone());
    let (input, adjusted) = midpoint(tokens.input_at(0), pos, 80 - SOURCE_LINE.len());

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
fn expect(tokens: &mut Lexer, tok: impl Into<Token>, msg: impl AsRef<str>) -> (usize, Token) {
    use std::mem::discriminant;
    let (pos, next) = tokens.next_token().expect("get next token");
    if discriminant(next) == discriminant(&tok.into()) {
        return (*pos, next.clone());
    }
    let pos = *pos;
    expect_fail(tokens.input_at(0), pos, msg.as_ref());
}

#[inline]
fn expect_type(tokens: &mut Lexer, msg: impl AsRef<str>) -> (usize, LexType) {
    let (pos, next) = tokens.next_token().expect("get next token");
    if let Token::Type(ty) = next {
        return (*pos, ty.clone());
    }
    let pos = *pos;
    expect_fail(tokens.input_at(0), pos, msg.as_ref());
}

#[inline]
fn expect_ident(tokens: &mut Lexer, msg: impl AsRef<str>) -> (usize, String) {
    let (pos, next) = tokens.next_token().expect("get next token");
    if let Token::Ident(name) = next {
        return (*pos, name.to_string());
    }
    let pos = *pos;
    expect_fail(tokens.input_at(0), pos, msg.as_ref());
}

fn expect_fail(input: &str, pos: usize, msg: &str) -> ! {
    const SOURCE_LINE: &str = "source=> ";
    let (input, adjusted) = midpoint(&input, pos, 80 - SOURCE_LINE.len());

    eprintln!("{}{}", wrap_color!(Color::Yellow {}, SOURCE_LINE), input);
    draw_caret(SOURCE_LINE.len() + adjusted, Color::Red {});

    fail!(
        "{} {} was expected. at position: {}.\n",
        wrap_color!(Color::Red {}, "ERROR:"),
        wrap_color!(Color::Cyan {}, msg),
        wrap_color!(Color::Blue {}, pos),
    );
}
