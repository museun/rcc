use super::*;
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct Var {
    ty: parser::Type, // ??
    offset: i32,
}

pub struct Semantics {
    map: HashMap<String, Var>, // variables
    stacksize: i32,
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

    fn walk(&mut self, node: &mut Node) {
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
                    self.walk(init.as_mut())
                }
            }

            Node::If { cond, body, else_ } => {
                self.walk(cond.as_mut());
                self.walk(body.as_mut());
                if else_.has_val() {
                    self.walk(else_.as_mut());
                }
            }
            Node::Else { body } => self.walk(body.as_mut()),

            Node::For {
                init,
                cond,
                step,
                body,
            } => {
                self.walk(init.as_mut());
                self.walk(cond.as_mut());
                self.walk(step.as_mut());
                self.walk(body.as_mut());
            }

            Node::Assign { lhs, rhs }
            | Node::LogAnd { lhs, rhs }
            | Node::LogOr { lhs, rhs }
            | Node::Comparison { lhs, rhs, .. } => {
                self.walk(lhs.as_mut());
                self.walk(rhs.as_mut());
            }

            Node::Mul { lhs, rhs, .. }
            | Node::Div { lhs, rhs, .. }
            | Node::Add { lhs, rhs, .. }
            | Node::Sub { lhs, rhs, .. } => {
                self.walk(lhs.as_mut());
                self.walk(rhs.as_mut());

                eprintln!("lhs: {:#?}", lhs);
                eprintln!("rhs: {:#?}", rhs);

                if let Type::Ptr { ptr: r } = rhs.get_type() {
                    if let Type::Ptr { ptr: l } = lhs.get_type() {
                        let r = *r.clone();
                        let l = *l.clone();
                        {
                            lhs.set_type(r);
                        }
                        {
                            rhs.set_type(l);
                        }
                    }
                }

                // TYPE: make sure its not circular pointers

                let ty = lhs.get_type().clone();
                node.set_type(ty);
            }

            Node::Deref { expr } => {
                self.walk(expr.as_mut());
                // TYPE: we don't flatten the pointers..
            }

            Node::Return { expr } => self.walk(expr.as_mut()),

            Node::Call { args, .. } => {
                for arg in args {
                    self.walk(arg.as_mut())
                }
                // TYPE: maybe set default type to int
            }

            Node::Func { args, body, .. } => {
                for arg in args {
                    self.walk(arg.as_mut())
                }
                self.walk(body.as_mut())
            }

            Node::Compound { stmts } => {
                for stmt in stmts {
                    self.walk(stmt.as_mut())
                }
            }

            Node::Statement { expr } => self.walk(expr.as_mut()),
            _ => fail!("unexpected node: {:?}", node),
        }
    }
}
