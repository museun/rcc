use super::*;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Var {
    pub(crate) ty: Type,
    pub(crate) offset: i32,
    pub(crate) global: Option<(String, String)>, // None = local
    pub(crate) data: i32,
    pub(crate) is_extern: bool,
}

pub struct Semantics<'a> {
    stacksize: i32,
    label: &'a mut u32,
    globals: Vec<Var>,
}

impl<'a> Semantics<'a> {
    pub fn analyze(nodes: &mut [Node]) -> (&mut [Node]) {
        let mut label = 0;
        let mut env = Environment::new(None);

        let mut globals = vec![];
        for mut node in nodes.iter_mut() {
            let mut this = Self {
                stacksize: 0,
                globals: vec![],
                label: &mut label,
            };

            if let Node::Vardef {
                name,
                ty,
                data,
                is_extern,
                ..
            } = &node
            {
                let var = Self::new_global(ty, &name, "", *data, *is_extern);
                globals.push(var.clone());
                env.insert(name.clone(), var);
                continue;
            }

            this.walk(&mut env, &mut node, true);

            if let Node::Func {
                stacksize,
                globals: v,
                ..
            } = &mut node
            {
                *stacksize = this.stacksize;
                v.extend(this.globals.clone());
                v.extend(globals.clone());
            }
        }

        nodes
    }

    fn new_global(ty: &Type, name: &str, s: &str, data: i32, is_extern: bool) -> Var {
        Var {
            ty: ty.clone(),
            offset: 0,
            global: Some((name.into(), s.into())),
            is_extern,
            data,
        }
    }

    fn check_lval(node: &Node) {
        match node {
            Node::LVal { .. } | Node::GVar { .. } | Node::Deref { .. } => {
                return;
            }
            _ => fail!("not an lvalue: {:?}", node),
        }
    }

    /// DANGER: this should be on a new node
    fn maybe_decay(&mut self, base: &mut Node, decay: bool) {
        if !decay {
            return;
        }

        if let Type::Array { .. } = base.get_type() {
            match base {
                Node::LVal { ref ty, .. } | Node::GVar { ref ty, .. } => {
                    *base = ty.addr_of(&base);
                }
                _ => fail!("must be a lvalue"),
            }
        }
    }

    // TODO maybe return a new node instead of mutating the current
    fn walk(&mut self, env: &mut Environment, node: &mut Node, decay: bool) {
        match node {
            Node::Constant { .. } => return,

            Node::Str { str, ty } => {
                let label = self.next_label();
                let var = Self::new_global(ty, &label, str, 0, false);
                self.globals.push(var);

                // this doesn't seem right
                *node = Node::GVar {
                    name: label,
                    ty: ty.clone(),
                };
                self.maybe_decay(node, decay);
            }

            Node::Ident { ref name } => {
                let var = env.find(name);
                if var.is_none() {
                    fail!("undefined variable: {}", name)
                }

                let var = var.unwrap();
                // local
                if var.global.is_none() {
                    *node = Node::LVal {
                        offset: var.offset,
                        ty: var.ty.clone(),
                    };
                    self.maybe_decay(node, decay);
                    return;
                }

                // globals
                *node = Node::GVar {
                    name: name.clone(),
                    ty: var.ty.clone(),
                };

                self.maybe_decay(node, decay);
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
                is_extern,
                data,
            } => {
                self.stacksize += ty.size_of();
                *offset = self.stacksize;

                env.insert(
                    name.clone(),
                    Var {
                        ty: ty.clone(),
                        offset: self.stacksize,
                        global: None,
                        is_extern: *is_extern,
                        data: *data,
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
                Self::check_lval(lhs.get_val());

                self.walk(env, rhs.as_mut(), true);
                let lhs = lhs.get_type().clone();
                node.set_type(lhs);
            }

            Node::LogAnd { lhs, rhs }
            | Node::LogOr { lhs, rhs }
            | Node::Equals { lhs, rhs }
            | Node::NEquals { lhs, rhs }
            | Node::Comparison { lhs, rhs, .. } => {
                self.walk(env, lhs.as_mut(), true);
                self.walk(env, rhs.as_mut(), true);

                // TYPE: implement types for this
            }

            Node::DoWhile { body, cond, .. } => {
                self.walk(env, cond.as_mut(), true);
                self.walk(env, body.as_mut(), true);
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
                Self::check_lval(expr.as_ref());
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
