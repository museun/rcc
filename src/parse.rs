use super::*;

type NodeKind = Option<Box<Node>>;

#[derive(Debug, PartialEq)]
pub enum Node {
    Constant {
        val: u32, // does this need the lhs?
    },
    Ident {
        name: String, // does this need the rhs?
    },

    // types
    Vardef {
        name: String,
        init: NodeKind,
    },

    Return {
        expr: NodeKind,
    },
    Assign {
        lhs: NodeKind,
        rhs: NodeKind,
    },
    LogAnd {
        lhs: NodeKind,
        rhs: NodeKind,
    },
    LogOr {
        lhs: NodeKind,
        rhs: NodeKind,
    },
    LessThan {
        lhs: NodeKind,
        rhs: NodeKind,
    },

    If {
        cond: NodeKind,
        body: NodeKind,
        else_: NodeKind,
    },
    Else {
        body: NodeKind,
    },

    For {
        init: NodeKind,
        cond: NodeKind,
        step: NodeKind,
        body: NodeKind,
    },

    Call {
        name: String,
        args: Vec<Node>,
    },
    Func {
        name: String,
        args: Vec<Node>,
        body: NodeKind,
    },

    Statement {
        expr: NodeKind,
    },

    Expression {
        lhs: NodeKind,
        rhs: NodeKind,
        tok: Token,
    },
    Compound {
        stmts: Vec<Node>,
    },
}

impl Node {
    pub fn parse(tokens: &mut Tokens) -> Vec<Self> {
        let mut nodes = vec![];
        while let Some((_, tok)) = tokens.peek() {
            if *tok == Token::EOF {
                break;
            }
            nodes.push(Self::function(tokens))
        }
        nodes
    }

    fn function(tokens: &mut Tokens) -> Self {
        let (_, _ty) = expect(tokens, Token::Int, "function return type");
        let (_, name) = expect_ident(tokens, "function name");
        let name = name.into();

        check(tokens, '(');
        let mut args = vec![];
        if !consume(tokens, ')') {
            args.push(Self::term(tokens));
            while consume(tokens, ',') {
                args.push(Self::term(tokens));
            }
            check(tokens, ')');
        }

        check(tokens, '{');
        Node::Func {
            name,
            args,
            body: make(Self::compound_stmt(tokens)),
        }
    }

    fn compound_stmt(tokens: &mut Tokens) -> Self {
        let mut stmts = vec![];
        while !consume(tokens, '}') {
            stmts.push(Self::stmt(tokens))
        }
        Node::Compound { stmts }
    }

    fn stmt(tokens: &mut Tokens) -> Self {
        match tokens.peek() {
            Some((_, Token::Int)) => Self::decl(tokens),
            Some((_, Token::If)) => {
                tokens.advance();
                check(tokens, '(');
                let cond = make(Self::assign(tokens));

                check(tokens, ')');
                let body = make(Self::stmt(tokens));

                let else_ = if consume(tokens, "else") {
                    make(Self::stmt(tokens))
                } else {
                    None
                };

                Node::If { cond, body, else_ }
            }
            Some((_, Token::For)) => {
                tokens.advance();
                check(tokens, '(');
                let init = if is_typename(tokens) {
                    Self::decl(tokens)
                } else {
                    Self::expr_stmt(tokens)
                };

                let cond = Self::assign(tokens);
                check(tokens, ';');

                let step = Self::assign(tokens);
                check(tokens, ')');

                let body = Self::stmt(tokens);
                Node::For {
                    init: make(init),
                    cond: make(cond),
                    step: make(step),
                    body: make(body),
                }
            }
            Some((_, Token::Return)) => {
                tokens.advance();
                let node = Node::Return {
                    expr: make(Node::assign(tokens)),
                };
                check(tokens, ';');
                node
            }
            Some((_, Token::OpenBrace)) => {
                tokens.advance();
                let mut stmts = vec![];
                while !consume(tokens, '}') {
                    stmts.push(Self::stmt(tokens));
                }
                Node::Compound { stmts }
            }
            Some((_, tok)) if *tok != Token::EOF => Self::expr_stmt(tokens),
            _ => fail!("unexpected token"),
        }
    }

    fn expr_stmt(tokens: &mut Tokens) -> Self {
        let node = Node::Statement {
            expr: make(Self::assign(tokens)),
        };
        check(tokens, ';');
        node
    }

    fn decl(tokens: &mut Tokens) -> Self {
        tokens.advance();
        let (_, name) = expect_ident(tokens, "variable name expected");
        let name = name.to_string();

        let init = if consume(tokens, '=') {
            make(Self::assign(tokens))
        } else {
            None
        };

        check(tokens, ';');
        Node::Vardef { name, init }
    }

    fn assign(tokens: &mut Tokens) -> Self {
        let lhs = Self::logor(tokens);
        if consume(tokens, '=') {
            return Node::Assign {
                lhs: make(lhs),
                rhs: make(Self::logor(tokens)),
            };
        }
        lhs
    }

    fn logor(tokens: &mut Tokens) -> Self {
        let mut lhs = Self::logand(tokens);
        'expr: loop {
            if let Some((_, Token::LogOr)) = tokens.peek() {
                tokens.advance();
                lhs = Node::LogOr {
                    lhs: make(lhs),
                    rhs: make(Self::logand(tokens)),
                };
            } else {
                break 'expr;
            }
        }
        lhs
    }

    fn logand(tokens: &mut Tokens) -> Self {
        let mut lhs = Self::rel(tokens);
        'expr: loop {
            if let Some((_, Token::LogAnd)) = tokens.peek() {
                tokens.advance();
                lhs = Node::LogAnd {
                    lhs: make(lhs),
                    rhs: make(Self::rel(tokens)),
                };
            } else {
                break 'expr;
            }
        }
        lhs
    }

    fn rel(tokens: &mut Tokens) -> Self {
        let mut lhs = Self::add(tokens);
        'expr: loop {
            match tokens.peek() {
                Some((_, Token::LessThan)) => {
                    tokens.advance();
                    lhs = Node::LessThan {
                        lhs: make(lhs),
                        rhs: make(Self::add(tokens)),
                    };
                }
                Some((_, Token::GreaterThan)) => {
                    tokens.advance();
                    lhs = Node::LessThan {
                        lhs: make(Self::add(tokens)),
                        rhs: make(lhs),
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
            match tokens.peek() {
                Some((_, tok @ Token::Add)) | Some((_, tok @ Token::Sub)) => {
                    let tok = tok.clone();
                    tokens.advance();
                    lhs = Node::Expression {
                        lhs: make(lhs),
                        rhs: make(Self::mul(tokens)),
                        tok,
                    };
                }
                _ => break 'expr,
            }
        }
        lhs
    }

    fn mul(tokens: &mut Tokens) -> Self {
        let mut lhs = Self::term(tokens);
        'expr: loop {
            match tokens.peek() {
                Some((_, tok @ Token::Mul)) | Some((_, tok @ Token::Div)) => {
                    let tok = tok.clone();
                    tokens.advance();
                    lhs = Node::Expression {
                        lhs: make(lhs),
                        rhs: make(Self::term(tokens)),
                        tok,
                    };
                }
                _ => break 'expr,
            }
        }
        lhs
    }

    fn term(tokens: &mut Tokens) -> Self {
        match tokens.next_token() {
            Some((_, Token::Num(n))) => Node::Constant { val: *n },
            Some((_, Token::Ident(ref name))) => {
                let n = name.clone();
                if !consume(tokens, '(') {
                    return Node::Ident { name: n };
                }

                if consume(tokens, ')') {
                    return Node::Call {
                        name: n,
                        args: vec![],
                    };
                }

                let mut args = vec![];
                args.push(Self::assign(tokens));
                while consume(tokens, ',') {
                    args.push(Self::assign(tokens));
                }
                check(tokens, ')');
                Node::Call { name: n, args }
            }
            Some((_, Token::OpenParen)) => {
                let node = Node::assign(tokens);
                check(tokens, ')');
                node
            }
            Some(tok) => fail!("number or ident expected at {}, but got: {}", tok.0, tok.1),
            _ => unreachable!(),
        }
    }
}

#[inline]
fn make(node: Node) -> Option<Box<Node>> {
    Some(Box::new(node))
}

/// this uses a discriminant comparison
#[inline]
fn expect<'a>(
    tokens: &'a mut Tokens,
    tok: impl Into<Token>,
    msg: impl AsRef<str>,
) -> (&'a usize, &'a Token) {
    let tok = tok.into();
    let cmp = ::std::mem::discriminant(&tok);
    match tokens.next_token() {
        Some((pos, t)) if ::std::mem::discriminant(t) != cmp => {
            fail!("{} expected at {}. but found: {}.", msg.as_ref(), pos, t)
        }
        Some((pos, t)) => (pos, t),
        _ => unreachable!(),
    }
}

#[inline]
fn expect_ident<'a>(tokens: &'a mut Tokens, msg: impl AsRef<str>) -> (&'a usize, &'a str) {
    match tokens.next_token() {
        Some((pos, Token::Ident(name))) => (pos, &name),
        Some((pos, t)) => fail!("{} expected at {}. but found: {}.", msg.as_ref(), pos, t),
        _ => unreachable!(),
    }
}

#[inline]
fn is_typename(tokens: &mut Tokens) -> bool {
    match tokens.peek() {
        Some((_, Token::Int)) => true,
        // TODO add more types
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

// TODO: this really needs a better name
#[inline]
fn check(tokens: &mut Tokens, tok: impl Into<Token>) {
    let tok = tok.into();
    match tokens.next_token() {
        Some((pos, t)) if *t != tok => {
            let (t, pos) = (t.clone(), *pos);
            let input = tokens.input_at(0);

            const MAX: usize = 60;
            const HALF: usize = MAX / 2;
            let (input, adjusted) = if input.len() > MAX {
                if pos < HALF {
                    (&input[..HALF], pos)
                } else {
                    (&input[pos - HALF..], HALF)
                }
            } else {
                (input, pos)
            };

            use Color::*;

            eprintln!("{}{}", wrap_color!(Yellow, "source=> "), input);
            draw_caret("source=> ".len() + adjusted, Red);

            fail!(
                "{} {} was expected. found {} at position: {}.\n",
                wrap_color!(Red, "ERROR:"),
                wrap_color!(Green, tok),
                wrap_color!(Red, t),
                wrap_color!(Blue, pos),
            );
        }
        _ => {}
    }
}

fn draw_caret(width: usize, color: Color) {
    let s = ::std::iter::repeat(" ").take(width).collect::<String>();
    eprintln!("{}{}", s, wrap_color!(color, "^"));
}
