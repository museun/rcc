use super::*;
use std::collections::HashMap;

pub struct Semantics {
    map: HashMap<String, Var>, // variables
    stacksize: i32,
}

#[derive(Debug, Clone)]
struct Var {
    ty: parser::Type, // ??
    offset: i32,
}

impl Semantics {
    pub fn analyze(nodes: &mut [Node]) -> &mut [Node] {
        for mut node in nodes.iter_mut() {
            let mut this = Self {
                map: HashMap::new(),
                stacksize: 0,
            };

            this.walk(&mut node);
            if let Node::Func { stacksize, .. } = &mut node {
                *stacksize = this.stacksize
            }
        }
        nodes
    }

    fn walk(&mut self, mut node: impl AsMut<Node>) {
        let node = node.as_mut();
        match node {
            Node::Constant { .. } => return,

            Node::Ident { name } => {
                if !self.map.contains_key(name) {
                    fail!("undefined variable: {}", name)
                }

                let var = self.map[name].clone(); // TODO: fix this
                *node = Node::LVal {
                    offset: var.offset,
                    ty: var.ty,
                }
            }

            Node::Vardef {
                name,
                init,
                offset,
                ty,
            } => {
                self.stacksize += 8;
                *offset = self.stacksize;

                self.map.insert(
                    name.clone(),
                    Var {
                        ty: ty.clone(),
                        offset: self.stacksize,
                    },
                );

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
            | Node::LogAnd { lhs, rhs }
            | Node::LogOr { lhs, rhs }
            | Node::Comparison { lhs, rhs, .. } => {
                self.walk(lhs);
                self.walk(rhs);
                // TYPE: can always get the type from the lhs
                //*ty = *lhs.get_type();
            }

            Node::Deref { expr } | Node::Return { expr } => self.walk(expr),

            Node::Call { args, .. } => {
                for arg in args {
                    self.walk(arg)
                }
                // TYPE: maybe set default type to int
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
