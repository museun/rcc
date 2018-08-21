use token::{Token, Tokens};

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

    If {
        cond: NodeKind,
        body: NodeKind,
        else_: NodeKind,
    },
    Else {
        body: NodeKind,
    },

    Compound {
        stmts: Vec<Node>,
    },
    Expression {
        lhs: NodeKind,
        rhs: NodeKind,
        tok: Token,
    },
}

impl Node {
    pub fn parse(tokens: &mut Tokens) -> Self {
        Self::compound_stmt(tokens)
    }

    // ident, etc
    fn term(tokens: &mut Tokens) -> Self {
        match tokens.next_token() {
            Some((_, Token::Num(n))) => Node::Constant { val: *n },
            Some((_, Token::Ident(name))) => Node::Ident { name: name.clone() },
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

    fn expr(tokens: &mut Tokens) -> Self {
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

    fn assign(tokens: &mut Tokens) -> Self {
        let lhs = Self::expr(tokens);
        if eat(tokens, &Token::Assign) {
            return Node::Assign {
                lhs: Some(Box::new(lhs)),
                rhs: Some(Box::new(Self::expr(tokens))),
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
            Some((_, Token::Ret)) => {
                tokens.advance();
                let node = Node::Return {
                    expr: Some(Box::new(Node::assign(tokens))),
                };
                check_tok(tokens, &Token::EOS);
                node
            }
            Some((_, tok)) if *tok != Token::EOF => {
                let tok = tok.clone();
                let node = Node::Expression {
                    lhs: Some(Box::new(Node::assign(tokens))),
                    rhs: None,
                    tok,
                };
                check_tok(tokens, &Token::EOS);
                node
            }
            Some((pos, tok)) => fail!("unexpected token at {}: {:?}", pos.clone(), tok),
            None => fail!("unexpected token"),
        }
    }

    fn compound_stmt(tokens: &mut Tokens) -> Self {
        let mut stmts = vec![];

        loop {
            match tokens.peek() {
                Some((_, tok)) if *tok == Token::EOF => return Node::Compound { stmts },
                _ => stmts.push(Self::stmt(tokens)),
            };
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
