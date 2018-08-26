use super::*;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Var {
    pub(crate) ty: Type,
    pub(crate) offset: i32,
    pub(crate) global: Option<(String, String)>, // None = local
}

pub struct Semantics<'a> {
    globals: Vec<Var>, // globals
    stacksize: i32,
    label: &'a mut u32,
}

impl<'a> Semantics<'a> {
    pub fn analyze(nodes: &mut [Node]) -> &mut [Node] {
        let mut label = 0;
        for mut node in nodes.iter_mut() {
            let mut this = Self {
                globals: vec![],
                stacksize: 0,
                label: &mut label,
            };

            let mut env = Environment::new(None);
            this.walk(&mut env, &mut node, true);
            if let Node::Func {
                stacksize, globals, ..
            } = &mut node
            {
                *stacksize = this.stacksize;
                *globals = this.globals.clone()
            }
        }
        nodes
    }

    // TODO maybe return a new node instead of mutating the current
    fn walk(&mut self, env: &mut Environment, node: &mut Node, decay: bool) {
        match node {
            Node::Constant { .. } => return,

            Node::Str { str, ty } => {
                let label = self.next_label();
                let var = Var {
                    ty: ty.clone(),
                    offset: 0,
                    global: Some((label.clone(), str.clone())),
                };

                self.globals.push(var);
                *node = Node::GVar {
                    name: label,
                    ty: ty.clone(),
                };
                self.walk(env, node, decay);
            }

            Node::Ident { ref name } => {
                let var = env.find(name);
                if var.is_none() {
                    fail!("undefined variable: {}", name)
                }

                let var = var.unwrap();
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
                    ty: var.ty.clone(),
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

                env.insert(
                    name.clone(),
                    Var {
                        ty: ty.clone(),
                        offset: self.stacksize,
                        global: None,
                    },
                );

                if init.has_val() {
                    self.walk(env, init.as_mut(), true)
                }
            }

            Node::If { cond, body, else_ } => {
                self.walk(env, cond.as_mut(), true);
                self.walk(env, body.as_mut(), true);
                if else_.has_val() {
                    self.walk(env, else_.as_mut(), true);
                }
            }

            Node::Else { body } => self.walk(env, body.as_mut(), true),

            Node::For {
                init,
                cond,
                step,
                body,
            } => {
                self.walk(env, init.as_mut(), true);
                self.walk(env, cond.as_mut(), true);
                self.walk(env, step.as_mut(), true);
                self.walk(env, body.as_mut(), true);
            }

            Node::Assign { lhs, rhs } => {
                self.walk(env, lhs.as_mut(), false); // can't decay this
                match lhs.get_val() {
                    Node::LVal { .. } | Node::Deref { .. } => {}
                    _ => fail!("not an lvalue: {:?}", node),
                }

                self.walk(env, rhs.as_mut(), true);
                let lhs = lhs.get_type().clone();
                node.set_type(lhs)
            }

            Node::LogAnd { lhs, rhs }
            | Node::LogOr { lhs, rhs }
            | Node::Comparison { lhs, rhs, .. } => {
                self.walk(env, lhs.as_mut(), true);
                self.walk(env, rhs.as_mut(), true);
            }

            Node::Mul { lhs, rhs, .. }
            | Node::Div { lhs, rhs, .. }
            | Node::Add { lhs, rhs, .. }
            | Node::Sub { lhs, rhs, .. } => {
                self.walk(env, lhs.as_mut(), true);
                self.walk(env, rhs.as_mut(), true);

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
                self.walk(env, expr.as_mut(), true);
                *ty = expr.get_type().ptr_of()
            }

            Node::Deref { expr } => {
                self.walk(env, expr.as_mut(), true);
                if !expr.get_type().is_ptr() {
                    fail!("operand must be a pointer");
                }

                let ptr = expr.get_type().clone();
                node.set_type(ptr);
            }

            Node::Return { expr } => self.walk(env, expr.as_mut(), true),

            Node::Sizeof { expr } => {
                self.walk(env, expr.as_mut(), false);
                *node = Node::Constant {
                    val: expr.get_type().size_of() as u32,
                    ty: Type::Int,
                };
            }

            Node::Call { args, .. } => {
                for arg in args {
                    self.walk(env, arg.as_mut(), true)
                }
                // TYPE: maybe set default type to int
            }

            Node::Func { args, body, .. } => {
                for arg in args {
                    self.walk(env, arg.as_mut(), true)
                }
                self.walk(env, body.as_mut(), true)
            }

            Node::Compound { stmts } => {
                let mut newenv = Environment::new(Some(env));

                for stmt in stmts {
                    self.walk(&mut newenv, stmt.as_mut(), true)
                }
            }

            Node::Statement { expr } => self.walk(env, expr.as_mut(), true),
            _ => fail!("unexpected node: {:?}", node),
        }
    }

    fn next_label(&mut self) -> String {
        let label = format!(".L.str{}", self.label);
        *self.label += 1;
        label
    }
}

pub struct Environment<'a> {
    vars: HashMap<String, Var>,
    prev: Option<&'a Environment<'a>>,
}

impl<'a> Environment<'a> {
    pub fn new(prev: Option<&'a Environment>) -> Self {
        Self {
            vars: HashMap::new(),
            prev,
        }
    }

    pub fn insert(&mut self, name: String, var: Var) {
        self.vars.insert(name, var);
    }

    pub fn find(&self, name: &str) -> Option<&Var> {
        if self.vars.contains_key(name) {
            Some(&self.vars[name])
        } else if let Some(prev) = self.prev {
            prev.find(name)
        } else {
            None
        }
    }
}
