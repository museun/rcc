use super::*;
use std::collections::HashMap;

pub struct Semantics {
    map: HashMap<String, i32>, // offsets
    stacksize: i32,
}

impl Semantics {
    pub fn analyze(mut nodes: Vec<Node>) -> Vec<Node> {
        let mut out = vec![];
        for mut node in &mut nodes {
            let mut this = Self {
                map: HashMap::new(),
                stacksize: 0,
            };

            this.walk(&mut node);
            if let Node::Func { stacksize, .. } = &mut node {
                *stacksize = this.stacksize
            }
            out.push(node.clone()); // will this break the references?
        }
        out
    }

    fn walk(&mut self, mut node: impl AsMut<Node>) {
        let node = node.as_mut();
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
            Node::Vardef {
                name, init, offset, ..
            } => {
                self.stacksize += 8;
                self.map.insert(name.clone(), self.stacksize);
                *offset = self.stacksize;
                if init.has_val() {
                    self.walk(init)
                }
            }
            Node::If { cond, body, else_ } => {
                self.walk(cond);
                self.walk(body);
                if else_.has_val() {
                    self.walk(else_);
                }
            }
            Node::Else { body } => self.walk(body),
            Node::For {
                init,
                cond,
                step,
                body,
            } => {
                self.walk(init);
                self.walk(cond);
                self.walk(step);
                self.walk(body);
            }

            Node::Add { lhs, rhs }
            | Node::Sub { lhs, rhs }
            | Node::Mul { lhs, rhs }
            | Node::Div { lhs, rhs }
            | Node::Assign { lhs, rhs }
            | Node::Comparison { lhs, rhs, .. }
            | Node::LogAnd { lhs, rhs }
            | Node::LogOr { lhs, rhs } => {
                self.walk(lhs);
                self.walk(rhs);
            }
            Node::Deref { expr } | Node::Return { expr } => self.walk(expr),
            Node::Call { args, .. } => {
                for arg in args {
                    self.walk(arg)
                }
            }
            Node::Func { args, body, .. } => {
                for arg in args {
                    self.walk(arg)
                }
                self.walk(body)
            }
            Node::Compound { stmts } => {
                for stmt in stmts {
                    self.walk(stmt)
                }
            }
            Node::Statement { expr } => self.walk(expr),
            _ => fail!("unexpected node: {:?}", node),
        }
    }
}
