use super::*;
use std::collections::HashMap;

pub struct Semantics {
    map: HashMap<String, i32>, // offsets
    stacksize: i32,
}

impl Semantics {
    pub fn analyze(nodes: &mut Vec<&mut Node>) -> Vec<Node> {
        let mut out = vec![];
        for mut node in nodes {
            let mut this = Self {
                map: HashMap::new(),
                stacksize: 0,
            };

            this.walk(&mut node);
            if let Node::Func { stacksize, .. } = node {
                *stacksize = this.stacksize
            }
            out.push(node.clone()); // will this break the references?
        }
        out
    }

    fn walk(&mut self, node: &mut Node) {
        match node {
            Node::Constant { .. } => return,
            Node::Ident { name } => {
                if !self.map.contains_key(name) {
                    fail!("undefined variable: {}", name)
                }
                *node = Node::LVal {
                    offset: self.map[name],
                }
            }
            Node::Vardef { name, init, offset } => {
                self.stacksize += 8;
                self.map.insert(name.clone(), self.stacksize);
                *offset = self.stacksize;
                if init.is_some() {
                    self.walk(init.as_mut().unwrap())
                }
            }
            Node::If { cond, body, else_ } => {
                self.walk(cond.as_mut().unwrap());
                self.walk(body.as_mut().unwrap());
                if else_.is_some() {
                    self.walk(else_.as_mut().unwrap());
                }
            }
            Node::Else { body } => self.walk(body.as_mut().unwrap()),
            Node::For {
                init,
                cond,
                step,
                body,
            } => {
                self.walk(init.as_mut().unwrap());
                self.walk(cond.as_mut().unwrap());
                self.walk(step.as_mut().unwrap());
                self.walk(body.as_mut().unwrap());
            }

            Node::Add { lhs, rhs }
            | Node::Sub { lhs, rhs }
            | Node::Mul { lhs, rhs }
            | Node::Div { lhs, rhs }
            | Node::Assign { lhs, rhs }
            | Node::Comparison { lhs, rhs }
            | Node::LogAnd { lhs, rhs }
            | Node::LogOr { lhs, rhs } => {
                self.walk(lhs.as_mut().unwrap());
                self.walk(rhs.as_mut().unwrap());
            }
            Node::Return { expr } => self.walk(expr.as_mut().unwrap()),
            Node::Call { args, .. } => {
                for arg in args {
                    self.walk(arg)
                }
            }
            Node::Func { args, body, .. } => {
                for arg in args {
                    self.walk(arg)
                }
                self.walk(body.as_mut().unwrap())
            }
            Node::Compound { stmts } => {
                for stmt in stmts {
                    self.walk(stmt)
                }
            }
            Node::Statement { expr } => self.walk(expr.as_mut().unwrap()),
            _ => fail!("unexpected node: {:?}", node),
        }
    }
}
