use token::{Token, Tokens};

#[derive(Debug, PartialEq, Clone)]
pub enum NodeType {
    Return,
    Compound,
    Assign,
    If,
    Else,
    Ident(String), // borrowed &str is out of the question
    Constant(u32),
    Expression(Token), // this is totally the wrong name for this
}

// TODO split this off into an enum
#[derive(Debug)]
pub struct Node {
    pub ty: NodeType,

    pub lhs: Option<Box<Node>>,
    pub rhs: Option<Box<Node>>,

    pub expr: Option<Box<Node>>,
    pub stmts: Vec<Node>,

    pub cond: Option<Box<Node>>,
    pub then: Option<Box<Node>>,
    pub else_: Option<Box<Node>>,
}

impl Default for Node {
    fn default() -> Self {
        Node {
            ty: NodeType::Return,
            lhs: None,
            rhs: None,
            expr: None,
            stmts: vec![],
            cond: None,
            then: None,
            else_: None,
        }
    }
}

impl Node {
    pub fn parse(tokens: &mut Tokens) -> Self {
        Self::compound_stmt(tokens)
    }

    fn new(ty: NodeType, lhs: Option<Node>, rhs: Option<Node>) -> Self {
        Self {
            ty,

            lhs: lhs.map(Box::new),
            rhs: rhs.map(Box::new),

            expr: None,
            stmts: vec![],

            cond: None,
            then: None,
            else_: None,
        }
    }

    // ident, etc
    fn term(tokens: &mut Tokens) -> Self {
        match tokens.next_token() {
            Some((_, Token::Num(n))) => Node::new(NodeType::Constant(*n), None, None),
            Some((_, Token::Ident(name))) => Node::new(NodeType::Ident(name.clone()), None, None),
            Some((_, Token::OpenParen)) => {
                let node = Node::assign(tokens);
                check_tok(tokens, &node, &Token::CloseParen);
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
                    lhs = Node::new(
                        NodeType::Expression(tok),
                        Some(lhs),
                        Some(Self::term(tokens)),
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

    fn assign(tokens: &mut Tokens) -> Self {
        let lhs = Self::expr(tokens);
        if eat(tokens, &Token::Assign) {
            return Node::new(NodeType::Assign, Some(lhs), Some(Self::expr(tokens)));
        }
        lhs
    }

    fn stmt(tokens: &mut Tokens) -> Self {
        fn make_node(ty: NodeType, tokens: &mut Tokens) -> Node {
            let mut node = Node::default();
            node.ty = ty;
            node.expr = Some(Box::new(Node::assign(tokens)));
            node
        }

        match tokens.peek() {
            Some((_, Token::If)) => {
                tokens.advance();
                let mut node = Node::default();
                // if
                node.ty = NodeType::If;

                // (
                check_tok(tokens, &node, &Token::OpenParen);
                node.cond = Some(Box::new(Self::assign(tokens)));
                // )
                check_tok(tokens, &node, &Token::CloseParen);
                node.then = Some(Box::new(Self::stmt(tokens)));

                // else
                if eat(tokens, &Token::Else) {
                    node.else_ = Some(Box::new(Self::stmt(tokens)));
                }

                node
            }
            Some((_, Token::Ret)) => {
                tokens.advance();
                let node = make_node(NodeType::Return, tokens);
                check_tok(tokens, &node, &Token::EOS);
                node
            }
            Some((_, tok)) if *tok != Token::EOF => {
                let node = make_node(NodeType::Expression(tok.clone()), tokens);
                check_tok(tokens, &node, &Token::EOS);
                node
            }
            Some((pos, tok)) => fail!("unexpected token at {}: {:?}", pos.clone(), tok),
            None => fail!("unexpected token"),
        }
    }

    fn compound_stmt(tokens: &mut Tokens) -> Self {
        let mut node = Node::default();
        node.ty = NodeType::Compound;

        loop {
            match tokens.peek() {
                Some((_, tok)) if *tok == Token::EOF => return node,
                _ => node.stmts.push(Self::stmt(tokens)),
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

fn check_tok(tokens: &mut Tokens, node: &Node, tok: &Token) {
    match tokens.next_token() {
        Some((pos, t)) if t != tok => fail!(
            "{} expected. {:?} found at {}. built: {:#?}",
            tok,
            t,
            pos,
            node
        ),
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    // #[ignore]
    fn test_parse() {
        let input = "a = 1+2+3; b = 1*2/2; return a + b;";
        let mut tokens = Tokens::tokenize(&input);
        eprintln!("{}", tokens);

        let nodes = Node::parse(&mut tokens);
        eprintln!("{:#?}", nodes);
    }
}
