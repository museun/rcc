use token::Token;

#[derive(Debug, PartialEq)]
pub enum NodeType {
    Add,
    Sub,
    Num(u32),
}

impl From<u32> for NodeType {
    fn from(data: u32) -> Self {
        NodeType::Num(data)
    }
}

#[derive(Debug)]
pub struct Node {
    pub ty: NodeType,
    pub lhs: Option<Box<Node>>,
    pub rhs: Option<Box<Node>>,
}

impl Node {
    pub fn new(ty: NodeType, lhs: Option<Node>, rhs: Option<Node>) -> Self {
        Self {
            ty,
            lhs: lhs.map(Box::new),
            rhs: rhs.map(Box::new),
        }
    }

    pub fn num(tokens: &mut impl Iterator<Item = Token>) -> Self {
        match tokens.next() {
            Some(Token::Num(n)) => Node::new(NodeType::Num(n), None, None),
            tok => fail!("number expected, but got: {:?}", tok),
        }
    }

    pub fn expr(tokens: &mut impl Iterator<Item = Token>) -> Self {
        let mut lhs = Self::num(tokens);
        'expr: loop {
            match tokens.next() {
                Some(Token::Add) => {
                    lhs = Node::new(NodeType::Add, Some(lhs), Some(Self::num(tokens)));
                }
                Some(Token::Sub) => {
                    lhs = Node::new(NodeType::Sub, Some(lhs), Some(Self::num(tokens)));
                }
                _ => break 'expr,
            }
        }

        // TODO replace this iterator with a struct that'll allow peeking
        // need to check to see if we're at the EOF

        lhs
    }
}
