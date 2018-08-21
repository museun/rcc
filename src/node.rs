use token::{Token, Tokens};

#[derive(Debug)]
pub struct Node {
    pub ty: Token,
    pub lhs: Option<Box<Node>>,
    pub rhs: Option<Box<Node>>,
}

impl Node {
    fn new(ty: Token, lhs: Option<Node>, rhs: Option<Node>) -> Self {
        Self {
            ty,
            lhs: lhs.map(Box::new),
            rhs: rhs.map(Box::new),
        }
    }

    fn num(tokens: &mut Tokens) -> Self {
        match tokens.next_token() {
            Some(tok @ Token::Num(_)) => Node::new(tok.clone(), None, None),
            tok => fail!("number expected, but got: {:?}", tok),
        }
    }

    fn mul(tokens: &mut Tokens) -> Self {
        let mut lhs = Self::num(tokens);
        'expr: loop {
            // this needs to peek
            match tokens.peek() {
                Some(tok @ Token::Mul) | Some(tok @ Token::Div) => {
                    let tok = tok.clone();
                    tokens.advance();
                    lhs = Node::new(tok, Some(lhs), Some(Self::num(tokens)));
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
                Some(tok @ Token::Add) | Some(tok @ Token::Sub) => {
                    let tok = tok.clone();
                    tokens.advance();
                    lhs = Node::new(tok, Some(lhs), Some(Self::mul(tokens)));
                }
                _ => break 'expr,
            }
        }
        lhs
    }

    pub fn parse(tokens: &mut Tokens) -> Self {
        let node = Self::expr(tokens);
        match tokens.peek() {
            Some(Token::EOF) => {}
            _ => fail!("stray tokens found"),
        }
        node
    }
}
