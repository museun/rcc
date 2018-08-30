use super::*;
use node::Node;
use types::{Type, Var};

use std::collections::HashMap;

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
                let var = Var::global(Rc::clone(&ty), &name, "", *data, *is_extern);
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
                v.extend(this.globals);
                v.extend(globals.clone());
            }
        }

        nodes
    }

    fn check_lval(node: &Node) {
        match node {
            Node::LVal { .. } | Node::GVar { .. } | Node::Deref { .. } | Node::Dot { .. } => {
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

        if let Type::Array { base: t, .. } = &*base.get_type().unwrap().borrow() {
            let addr = types::addr_of(Rc::clone(&t), &base);
            *base = addr;
        }
    }

    // TODO maybe return a new node instead of mutating the current
    fn walk(&mut self, env: &mut Environment, node: &mut Node, decay: bool) {
        match node {
            Node::Constant { .. } => return,

            Node::Str { str, ty } => {
                let label = self.next_label();
                let var = Var::global(Rc::clone(&ty), &label, str, 0, false);
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

                // global
                *node = Node::GVar {
                    name: name.to_string(),
                    ty: var.ty.clone(),
                };

                self.maybe_decay(node, decay);
            }

            // Node::GVar { ref ty, .. } => {
            //     if decay {
            //         if let Type::Array { .. } = &*ty.borrow() {
            //             *node = types::addr_of(Rc::clone(&ty), &node);
            //         }
            //     }
            // }
            Node::Vardef {
                name,
                init,
                offset,
                ty,
                is_extern,
                data,
            } => {
                self.stacksize = round(self.stacksize, types::align_of(&*ty.borrow()));
                self.stacksize += types::size_of(&*ty.borrow());
                *offset = self.stacksize;

                env.insert(
                    name.to_string(),
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
                if init.has_val() {
                    self.walk(env, init.as_mut(), true);
                }
                self.walk(env, cond.as_mut(), true);
                if step.has_val() {
                    self.walk(env, step.as_mut(), true);
                }
                self.walk(env, body.as_mut(), true);
            }

            Node::DoWhile { body, cond, .. } => {
                self.walk(env, cond.as_mut(), true);
                self.walk(env, body.as_mut(), true);
            }

            Node::Assign { lhs, rhs } => {
                self.walk(env, lhs.as_mut(), false); // can't decay this
                Self::check_lval(lhs.get_val());
                self.walk(env, rhs.as_mut(), true);

                let ty = Rc::clone(lhs.get_type().as_ref().unwrap());
                node.set_type(ty);
            }

            Node::Dot { expr, name, offset } => {
                self.walk(env, expr.as_mut(), true);

                if let Type::Struct { ref members, .. } =
                    &*expr.get_type().as_ref().unwrap().borrow()
                {
                    let mut typ = None;
                    for member in members {
                        if let Node::Vardef {
                            name: mname,
                            ty,
                            offset: ref off,
                            ..
                        } = member
                        {
                            if *mname != *name {
                                continue;
                            }
                            *offset = *off;
                            typ = Some(ty.clone())
                        }
                    }

                    node.set_type(typ.unwrap());
                    return self.maybe_decay(node, decay);
                } else {
                    fail!("struct expected before . operator")
                }

                // fail!("no member '{}' on struct", name)
            }

            Node::LogAnd { lhs, rhs }
            | Node::LogOr { lhs, rhs }
            | Node::Equals { lhs, rhs }
            | Node::NEquals { lhs, rhs }
            | Node::Comparison { lhs, rhs, .. } => {
                self.walk(env, lhs.as_mut(), true);
                self.walk(env, rhs.as_mut(), true);

                // let ty = lhs.get_type().clone();
                // node.set_type(ty);
            }

            Node::Mul { lhs, rhs, .. }
            | Node::Div { lhs, rhs, .. }
            | Node::Add { lhs, rhs, .. }
            | Node::Sub { lhs, rhs, .. } => {
                self.walk(env, lhs.as_mut(), true);
                self.walk(env, rhs.as_mut(), true);

                // XXX: this is awful
                if let Type::Ptr { ptr: r, .. } = &*rhs.get_type().as_ref().unwrap().borrow() {
                    if let Type::Ptr { ptr: l, .. } = &*lhs.get_type().as_ref().unwrap().borrow() {
                        lhs.set_type(Rc::clone(&r));
                        rhs.set_type(Rc::clone(&l));
                    }
                }

                // TYPE: make sure its not circular pointers
                let ty = Rc::clone(lhs.get_type().as_ref().unwrap());
                node.set_type(ty);
            }

            Node::Addr { expr, ty: _ty } => {
                self.walk(env, expr.as_mut(), true);
                Self::check_lval(expr.as_ref());

                let ptr = types::ptr_of(Rc::clone(&expr.get_type().as_ref().unwrap()));
                node.set_type(Rc::new(RefCell::new(ptr)));
            }

            Node::Deref { expr } => {
                self.walk(env, expr.as_mut(), true);

                match types::as_ptr(Rc::clone(&expr.get_type().as_ref().unwrap())) {
                    Some(ty) => node.set_type(Rc::clone(&ty)),
                    None => fail!("operand must be a pointer"),
                };
            }

            Node::Return { expr } => self.walk(env, expr.as_mut(), true),

            Node::Sizeof { expr } => {
                self.walk(env, expr.as_mut(), false);
                *node = Node::Constant {
                    val: types::size_of(&*expr.get_type().as_ref().unwrap().borrow()) as u32,
                    ty: Rc::new(RefCell::new(Type::Int)),
                };
            }

            Node::Alignof { expr } => {
                self.walk(env, expr.as_mut(), false);
                *node = Node::Constant {
                    val: types::align_of(&*expr.get_type().as_ref().unwrap().borrow()) as u32,
                    ty: Rc::new(RefCell::new(Type::Int)),
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

            Node::Expression { expr } => self.walk(env, expr.as_mut(), true),
            Node::Statement { stmt, ty: _ty } => {
                self.walk(env, stmt.as_mut(), true);
                node.set_type(Rc::new(RefCell::new(Type::Int)))
            }
            Node::Noop {} => {}
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

    pub fn prev(&self) -> Option<&'a Environment<'a>> {
        self.prev
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
