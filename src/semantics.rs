use super::*;
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct Var {
    ty: Type,
    offset: i32,
    global: Option<String>,
}

pub struct Semantics<'a> {
    map: HashMap<String, Var>, // variables
    strings: Vec<(String, String)>,
    stacksize: i32,
    string_label: &'a mut u32,
}

impl<'a> Semantics<'a> {
    pub fn analyze(nodes: &mut [Node]) -> &mut [Node] {
        let mut label = 0;
        for mut node in nodes.iter_mut() {
            let mut this = Self {
                map: HashMap::new(),
                strings: vec![],
                stacksize: 0,
                string_label: &mut label,
            };

            this.walk(&mut node, true);
            if let Node::Func {
                stacksize, strings, ..
            } = &mut node
            {
                *stacksize = this.stacksize;
                *strings = this.strings.clone()
            }
        }
        nodes
    }

    // TODO maybe return a new node instead of mutating the current
    fn walk(&mut self, node: &mut Node, decay: bool) {
        match node {
            Node::Constant { .. } => return,

            Node::Str { str, ty } => {
                let label = self.next_string_label();
                self.strings.push((label.clone(), str.clone()));
                *node = Node::GVar {
                    name: label,
                    ty: ty.clone(),
                };
                self.walk(node, decay);
            }

            Node::Ident { ref name } => {
                if !self.map.contains_key(name) {
                    fail!("undefined variable: {}", name)
                }

                let var = self.map[name].clone();
                if decay {
                    if let Type::Array { .. } = var.ty {
                        *node = var.ty.addr_of(&Node::LVal {
                            offset: var.offset,
                            ty: var.ty.clone(),
                        });
                        return;
                    }
                }
                *node = Node::LVal {
                    offset: var.offset,
                    ty: var.ty,
                }
            }

            Node::GVar { ref ty, .. } => {
                if decay {
                    if let Type::Array { .. } = ty {
                        *node = ty.addr_of(&node);
                    }
                }
            }

            Node::Vardef {
                name,
                init,
                offset,
                ty,
            } => {
                self.stacksize += ty.size_of();
                *offset = self.stacksize;

                self.map.insert(
                    name.clone(),
                    Var {
                        ty: ty.clone(),
                        offset: self.stacksize,
                        global: None,
                    },
                );

                if init.has_val() {
                    self.walk(init.as_mut(), true)
                }
            }

            Node::If { cond, body, else_ } => {
                self.walk(cond.as_mut(), true);
                self.walk(body.as_mut(), true);
                if else_.has_val() {
                    self.walk(else_.as_mut(), true);
                }
            }

            Node::Else { body } => self.walk(body.as_mut(), true),

            Node::For {
                init,
                cond,
                step,
                body,
            } => {
                self.walk(init.as_mut(), true);
                self.walk(cond.as_mut(), true);
                self.walk(step.as_mut(), true);
                self.walk(body.as_mut(), true);
            }

            Node::Assign { lhs, rhs } => {
                self.walk(lhs.as_mut(), false); // can't decay this
                match lhs.get_val() {
                    Node::LVal { .. } | Node::Deref { .. } => {}
                    _ => fail!("not an lvalue: {:?}", node),
                }

                self.walk(rhs.as_mut(), true);
                let lhs = lhs.get_type().clone();
                node.set_type(lhs)
            }

            Node::LogAnd { lhs, rhs }
            | Node::LogOr { lhs, rhs }
            | Node::Comparison { lhs, rhs, .. } => {
                self.walk(lhs.as_mut(), true);
                self.walk(rhs.as_mut(), true);
            }

            Node::Mul { lhs, rhs, .. }
            | Node::Div { lhs, rhs, .. }
            | Node::Add { lhs, rhs, .. }
            | Node::Sub { lhs, rhs, .. } => {
                self.walk(lhs.as_mut(), true);
                self.walk(rhs.as_mut(), true);

                if let Type::Ptr { ptr: r } = rhs.get_type() {
                    if let Type::Ptr { ptr: l } = lhs.get_type() {
                        let (r, l) = (*r.clone(), *l.clone());
                        lhs.set_type(r);
                        rhs.set_type(l);
                    }
                }

                // TYPE: make sure its not circular pointers
                let ty = lhs.get_type().clone();
                node.set_type(ty);
            }

            Node::Addr { expr, ty } => {
                self.walk(expr.as_mut(), true);
                *ty = expr.get_type().ptr_of()
            }

            Node::Deref { expr } => {
                self.walk(expr.as_mut(), true);
                if !expr.get_type().is_ptr() {
                    fail!("operand must be a pointer");
                }

                let ptr = expr.get_type().clone();
                node.set_type(ptr);
            }

            Node::Return { expr } => self.walk(expr.as_mut(), true),

            Node::Sizeof { expr } => {
                self.walk(expr.as_mut(), false);
                *node = Node::Constant {
                    val: expr.get_type().size_of() as u32,
                    ty: Type::Int,
                };
            }

            Node::Call { args, .. } => {
                for arg in args {
                    self.walk(arg.as_mut(), true)
                }
                // TYPE: maybe set default type to int
            }

            Node::Func { args, body, .. } => {
                for arg in args {
                    self.walk(arg.as_mut(), true)
                }
                self.walk(body.as_mut(), true)
            }

            Node::Compound { stmts } => {
                for stmt in stmts {
                    self.walk(stmt.as_mut(), true)
                }
            }

            Node::Statement { expr } => self.walk(expr.as_mut(), true),
            _ => fail!("unexpected node: {:?}", node),
        }
    }

    fn next_string_label(&mut self) -> String {
        let label = format!(".L.str{}", self.string_label);
        *self.string_label += 1;
        label
    }
}
