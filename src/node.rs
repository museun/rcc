use token::{Token, Tokens};

#[derive(Debug, PartialEq, Clone)]
pub enum NodeType {
    Return,
    Compound,
    Expression(Token), // this is totally the wrong name for this
    Constant(u32),
}

#[derive(Debug)]
pub struct Node {
    pub ty: NodeType,

    // what to do about these? they should be in the expression type..
    pub lhs: Option<Box<Node>>,
    pub rhs: Option<Box<Node>>,

    pub expr: Option<Box<Node>>,

    // XXX: is this box really needed?
    pub stmts: Vec<Box<Node>>,
}

impl Node {
    fn new(ty: NodeType, lhs: Option<Node>, rhs: Option<Node>) -> Self {
        Self {
            ty,

            lhs: lhs.map(Box::new),
            rhs: rhs.map(Box::new),

            expr: None,
            stmts: vec![],
        }
    }

    fn num(tokens: &mut Tokens) -> Self {
        match tokens.next_token() {
            Some((_, Token::Num(n))) => Node::new(NodeType::Constant(*n), None, None),
            tok => fail!("number expected, but got: {:?}", tok),
        }
    }

    fn mul(tokens: &mut Tokens) -> Self {
        let mut lhs = Self::num(tokens);
        'expr: loop {
            match tokens.peek() {
                Some((_, tok @ Token::Mul)) | Some((_, tok @ Token::Div)) => {
                    let tok = tok.clone();
                    tokens.advance();
                    lhs = Node::new(
                        NodeType::Expression(tok),
                        Some(lhs),
                        Some(Self::num(tokens)),
                    );
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
                    lhs = Node::new(
                        NodeType::Expression(tok),
                        Some(lhs),
                        Some(Self::mul(tokens)),
                    );
                }
                _ => break 'expr,
            }
        }
        lhs
    }

    fn stmt(tokens: &mut Tokens) -> Self {
        let mut node = Node {
            ty: NodeType::Compound,
            lhs: None,
            rhs: None,
            expr: None,
            stmts: vec![],
        };

        loop {
            match tokens.peek() {
                Some((_, Token::EOF)) => return node,
                Some((_, Token::Ret)) => {
                    tokens.advance();
                    let e = Node {
                        ty: NodeType::Return,
                        lhs: None,
                        rhs: None,
                        expr: Some(Box::new(Self::expr(tokens))),
                        stmts: vec![],
                    };
                    node.stmts.push(Box::new(e));
                    Self::check_eos(tokens);
                }
                Some((_, tok)) => {
                    let e = Node {
                        ty: NodeType::Expression(tok.clone()),
                        lhs: None,
                        rhs: None,
                        expr: Some(Box::new(Self::expr(tokens))),
                        stmts: vec![],
                    };
                    node.stmts.push(Box::new(e));
                    Self::check_eos(tokens);
                }
                None => return node,
            }
        }
    }

    fn check_eos(tokens: &mut Tokens) {
        match tokens.next_token() {
            Some((_, Token::Eos)) => {}
            Some((pos, tok)) => fail!("; expected. {:?} found at {}", tok, pos),
            _ => fail!("unexpected end"),
        }
    }

    pub fn parse(tokens: &mut Tokens) -> Self {
        Self::stmt(tokens)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore]
    fn test_parse() {
        let input = "return 1+2*3/4+5;";
        let mut tokens = Tokens::tokenize(&input);
        eprintln!("{:#?}", tokens);

        let nodes = Node::parse(&mut tokens);
        eprintln!("{:#?}", nodes);
    }
}
