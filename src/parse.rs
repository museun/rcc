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

    // ident, etc
    fn term(tokens: &mut Tokens) -> Self {
        match tokens.next_token() {
            Some((_, Token::Num(n))) => Node::Constant { val: *n },
            Some((_, Token::Ident(ref name))) => {
                let n = name.clone();
                if !eat(tokens, &Token::OpenParen) {
                    return Node::Ident { name: n };
                }

                if eat(tokens, &Token::CloseParen) {
                    return Node::Call {
                        name: n,
                        args: vec![],
                    };
                }

                let mut args = vec![];
                args.push(Self::assign(tokens));
                while eat(tokens, &Token::Comma) {
                    args.push(Self::assign(tokens));
                }
                check_tok(tokens, &Token::CloseParen);
                Node::Call { name: n, args }
            }
            Some((_, Token::OpenParen)) => {
                let node = Node::assign(tokens);
                check_tok(tokens, &Token::CloseParen);
                node
            }
            tok => fail!("number or ident expected, but got: {:?}", tok),
        }
    }

    fn mul(tokens: &mut Tokens) -> Self {
        let mut lhs = Self::term(tokens);
        'expr: loop {
            match tokens.peek() {
                Some((_, tok @ Token::Mul)) | Some((_, tok @ Token::Div)) => {
                    let tok = tok.clone();
                    tokens.advance();
                    lhs = Node::Expression {
                        lhs: Some(Box::new(lhs)),
                        rhs: Some(Box::new(Self::term(tokens))),
                        tok,
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
                        lhs: Some(Box::new(lhs)),
                        rhs: Some(Box::new(Self::mul(tokens))),
                        tok,
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
            match tokens.peek() {
                Some((_, Token::LessThan)) => {
                    tokens.advance();
                    lhs = Node::LessThan {
                        lhs: Some(Box::new(lhs)),
                        rhs: Some(Box::new(Self::add(tokens))),
                    };
                }
                Some((_, Token::GreaterThan)) => {
                    tokens.advance();
                    lhs = Node::LessThan {
                        lhs: Some(Box::new(Self::add(tokens))),
                        rhs: Some(Box::new(lhs)),
                    };
                }
                _ => break 'expr,
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
                    lhs: Some(Box::new(lhs)),
                    rhs: Some(Box::new(Self::rel(tokens))),
                };
            } else {
                break 'expr;
            }
        }
        lhs
    }

    fn logor(tokens: &mut Tokens) -> Self {
        let mut lhs = Self::logand(tokens);
        'expr: loop {
            if let Some((_, Token::LogOr)) = tokens.peek() {
                tokens.advance();
                lhs = Node::LogOr {
                    lhs: Some(Box::new(lhs)),
                    rhs: Some(Box::new(Self::logand(tokens))),
                };
            } else {
                break 'expr;
            }
        }
        lhs
    }

    fn assign(tokens: &mut Tokens) -> Self {
        let lhs = Self::logor(tokens);
        if eat(tokens, &Token::Assign) {
            return Node::Assign {
                lhs: Some(Box::new(lhs)),
                rhs: Some(Box::new(Self::logor(tokens))),
            };
        }
        lhs
    }

    fn stmt(tokens: &mut Tokens) -> Self {
        match tokens.peek() {
            Some((_, Token::If)) => {
                tokens.advance();
                check_tok(tokens, &Token::OpenParen);
                let cond = Some(Box::new(Self::assign(tokens)));

                check_tok(tokens, &Token::CloseParen);
                let body = Some(Box::new(Self::stmt(tokens)));

                let else_ = if eat(tokens, &Token::Else) {
                    Some(Box::new(Self::stmt(tokens)))
                } else {
                    None
                };

                Node::If { cond, body, else_ }
            }
            Some((_, Token::For)) => {
                tokens.advance();
                check_tok(tokens, &Token::OpenParen);
                let init = Self::assign(tokens);

                check_tok(tokens, &Token::Semicolon);
                let cond = Self::assign(tokens);

                check_tok(tokens, &Token::Semicolon);
                let step = Self::assign(tokens);

                check_tok(tokens, &Token::CloseParen);
                let body = Self::stmt(tokens);

                Node::For {
                    init: Some(Box::new(init)),
                    cond: Some(Box::new(cond)),
                    step: Some(Box::new(step)),
                    body: Some(Box::new(body)),
                }
            }
            Some((_, Token::Return)) => {
                tokens.advance();
                let node = Node::Return {
                    expr: Some(Box::new(Node::assign(tokens))),
                };
                check_tok(tokens, &Token::Semicolon);
                node
            }
            Some((_, Token::OpenBrace)) => {
                tokens.advance();
                let mut stmts = vec![];
                while !eat(tokens, &Token::CloseBrace) {
                    stmts.push(Self::stmt(tokens));
                }
                Node::Compound { stmts }
            }
            Some((_, tok)) if *tok != Token::EOF => {
                let tok = tok.clone();
                let node = Node::Expression {
                    lhs: Some(Box::new(Node::assign(tokens))),
                    rhs: None,
                    tok,
                };
                check_tok(tokens, &Token::Semicolon);
                node
            }
            Some((pos, tok)) => fail!("unexpected token at {}: {:?}", pos.clone(), tok),
            None => fail!("unexpected token"),
        }
    }

    fn compound_stmt(tokens: &mut Tokens) -> Self {
        let mut stmts = vec![];
        while !eat(tokens, &Token::CloseBrace) {
            stmts.push(Self::stmt(tokens))
        }
        Node::Compound { stmts }
    }

    fn function(tokens: &mut Tokens) -> Self {
        match tokens.peek() {
            Some((_, Token::Ident(ref name))) => {
                let name = name.clone();
                tokens.advance();
                check_tok(tokens, &Token::OpenParen);
                let mut args = vec![];
                if !eat(tokens, &Token::CloseParen) {
                    args.push(Self::term(tokens));
                    while eat(tokens, &Token::Comma) {
                        args.push(Self::term(tokens));
                    }
                    check_tok(tokens, &Token::CloseParen);
                }
                check_tok(tokens, &Token::OpenBrace);
                Node::Func {
                    name,
                    args,
                    body: Some(Box::new(Self::compound_stmt(tokens))),
                }
            }
            Some((pos, tok)) => {
                fail!(
                    "function name expected at {}, but got: {:?}",
                    pos.clone(),
                    tok.clone()
                );
            }
            _ => unreachable!(), // should be..
        }
    }
}

fn eat(tokens: &mut Tokens, tok: &Token) -> bool {
    match tokens.peek() {
        Some((_, t)) if t == tok => {
            tokens.advance();
            true
        }
        _ => false,
    }
}

fn check_tok(tokens: &mut Tokens, tok: &Token) {
    match tokens.next_token() {
        Some((pos, t)) if t != tok => fail!("{} expected. {:?} found at {}.", tok, t, pos,),
        _ => {}
    }
}
