use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub enum NodeType {
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
    pub lhs: Rc<Node>,
    pub rhs: Rc<Node>,
}

// can't root the graph in `main`
impl Node {
    pub fn new(ty: NodeType, lhs: Rc<Node>, rhs: Rc<Node>) -> Self {
        Self { ty, lhs, rhs }
    }
}
