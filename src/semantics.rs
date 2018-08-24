use super::*;
use std::collections::HashMap;

macro_rules! as_mut {
    ($e:expr) => {
        $e.as_mut().unwrap()
    };
}

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
            Node::Vardef {
                name, init, offset, ..
            } => {
                self.stacksize += 8;
                self.map.insert(name.clone(), self.stacksize);
                *offset = self.stacksize;
                if init.is_some() {
                    self.walk(as_mut!(init))
                }
            }
            Node::If { cond, body, else_ } => {
                self.walk(as_mut!(cond));
                self.walk(as_mut!(body));
                if else_.is_some() {
                    self.walk(as_mut!(else_));
                }
            }
            Node::Else { body } => self.walk(as_mut!(body)),
            Node::For {
                init,
                cond,
                step,
                body,
            } => {
                self.walk(as_mut!(init));
                self.walk(as_mut!(cond));
                self.walk(as_mut!(step));
                self.walk(as_mut!(body));
            }

            Node::Add { lhs, rhs }
            | Node::Sub { lhs, rhs }
            | Node::Mul { lhs, rhs }
            | Node::Div { lhs, rhs }
            | Node::Assign { lhs, rhs }
            | Node::Comparison { lhs, rhs, .. }
            | Node::LogAnd { lhs, rhs }
            | Node::LogOr { lhs, rhs } => {
                self.walk(as_mut!(lhs));
                self.walk(as_mut!(rhs));
            }
            Node::Deref { expr } | Node::Return { expr } => self.walk(as_mut!(expr)),
            Node::Call { args, .. } => {
                for arg in args {
                    self.walk(arg)
                }
            }
            Node::Func { args, body, .. } => {
                for arg in args {
                    self.walk(arg)
                }
                self.walk(as_mut!(body))
            }
            Node::Compound { stmts } => {
                for stmt in stmts {
                    self.walk(stmt)
                }
            }
            Node::Statement { expr } => self.walk(as_mut!(expr)),
            _ => fail!("unexpected node: {:?}", node),
        }
    }
}
