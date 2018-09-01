use super::*;
use kind::Kind;
use node::{Comp, Node};
use types::{Type, Var};

use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum IRType {
    RegReg {
        dst: i32,
        src: i32,
    },
    RegImm {
        reg: i32,
        val: i32,
    },
    RegLabel {
        reg: i32,
        label: String,
    },
    Reg {
        src: i32,
    },
    Imm {
        val: i32,
    },
    Call {
        reg: i32,
        name: String,
        args: Vec<i32>,
    },
    Cmp {
        cmp: Comp,
        dst: i32,
        src: i32,
    },
    Nop,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IR {
    pub ty: IRType,
    pub kind: IRKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IRKind {
    Imm,    // reg->imm
    Mov,    // reg->reg
    Return, // reg

    Load(Width),     // reg->reg
    Store(Width),    // reg->reg
    StoreArg(Width), // reg->reg

    Unless, // reg->imm
    Label,  // imm OR reg_label
    Jmp,    // imm
    If,     // reg->imm

    BpRel, // reg->imm

    Add, // reg->reg OR reg->imm
    Sub, // reg->reg OR reg->imm
    Mul, // reg->reg OR reg->imm
    Div, // reg->reg
    Mod, // reg->reg

    Shr, // reg->reg
    Shl, // reg->reg

    Or,  // reg->reg
    Xor, // reg->reg
    And, // reg->reg

    Neg, // reg

    Comparison, // cmp, reg->reg

    Kill, // reg
    Nop,  // nothing
    Call, // call name, [args]
}

#[derive(Debug, Clone, PartialEq)]
pub enum Width {
    W8,
    W32,
    W64,
}

impl fmt::Display for Width {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let w = match self {
            Width::W8 => "8",
            Width::W32 => "32",
            Width::W64 => "64",
        };
        write!(f, "{}", w)
    }
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub stacksize: i32,
    pub ir: Vec<IR>,
    pub globals: Vec<Var>,
}

#[derive(Debug)]
pub struct Generate<'a> {
    inst: Vec<IR>,

    label: &'a mut i32,

    // why are these mutable references
    reg: i32,
    ret_label: i32,
    ret_reg: i32,
}

impl<'a> Generate<'a> {
    pub fn generate(nodes: &[Node]) -> Vec<Function> {
        let mut label = 1;
        let mut out = vec![];

        for node in nodes {
            match node {
                Node::Vardef { .. } => {}
                Node::Func {
                    name,
                    body,
                    args,
                    stacksize,
                    ty: _ty,
                    globals,
                } => {
                    let mut this = Self {
                        // TODO be smarter about this
                        inst: Vec::with_capacity(1 << 12),
                        label: &mut label,
                        ret_label: 0,
                        ret_reg: 0,
                        reg: 1,
                    };

                    for (i, arg) in args.iter().enumerate() {
                        let offset = match arg.get_val() {
                            Node::LVal { offset, .. } => offset,
                            Node::Vardef { offset, .. } => offset,
                            _ => unreachable!(),
                        };

                        this.storearg(
                            &*arg.get_type().as_ref().unwrap().borrow(),
                            reg_imm(i as i32, *offset),
                        );
                    }

                    this.statement(body);

                    let function = Function {
                        name: name.to_string(),
                        stacksize: *stacksize,
                        ir: this.inst,
                        globals: globals.clone(),
                    };
                    out.push(function);
                }
                Node::Noop {} => {}
                node => fail!("expected a function node, got: {:?} ", node),
            }
        }
        out
    }

    fn statement(&mut self, node: impl AsRef<Node>) {
        match &node.as_ref() {
            Node::Vardef { init, offset, .. } => {
                if !init.has_val() {
                    return;
                }

                let rhs = self.expression(init);
                let lhs = self.next_reg();

                self.create(IRKind::BpRel, reg_imm(lhs, *offset));

                self.store(
                    &*node.as_ref().get_type().as_ref().unwrap().borrow(),
                    reg_reg(lhs, rhs),
                );

                self.kill(lhs);
                self.kill(rhs);
            }

            Node::If { cond, body, else_ } => {
                let r = self.expression(cond);
                let x = self.next_label();

                self.create(IRKind::Unless, reg_imm(r, x));
                self.kill(r);
                self.statement(body);

                if !else_.has_val() {
                    self.create(IRKind::Label, imm(x));
                    return;
                }

                let y = self.next_label();
                self.create(IRKind::Jmp, imm(y));
                self.create(IRKind::Label, imm(x));

                self.statement(else_);
                self.create(IRKind::Label, imm(y));
            }

            Node::DoWhile { cond, body } => {
                let x = self.next_label();
                self.create(IRKind::Label, imm(x));
                self.statement(body);

                let r = self.expression(cond);
                self.create(IRKind::If, reg_imm(r, x));
                self.kill(r);
            }

            Node::For {
                init,
                cond,
                step,
                body,
            } => {
                let x = self.next_label();
                let y = self.next_label();

                if init.has_val() {
                    self.statement(init);
                }
                self.create(IRKind::Label, imm(x));

                let r = self.expression(cond);
                self.create(IRKind::Unless, reg_imm(r, y));
                self.kill(r);
                self.statement(body);

                if step.has_val() {
                    self.statement(step);
                }
                self.create(IRKind::Jmp, imm(x));
                self.create(IRKind::Label, imm(y));
            }

            Node::Return { expr } => {
                let r = self.expression(expr);
                if self.ret_label != 0 {
                    self.create(IRKind::Mov, reg_reg(self.ret_reg, r));
                    self.kill(r);
                    self.create(IRKind::Jmp, imm(self.ret_label));
                    return;
                }
                self.create(IRKind::Return, reg(r));
                self.kill(r);
            }

            Node::Expression { expr } => {
                let r = self.expression(expr);
                self.kill(r);
            }

            Node::Compound { ref stmts } => {
                for stmt in stmts {
                    self.statement(stmt)
                }
            }

            Node::Noop {} => {}

            // TODO make this return a Result so we can print out an instruction trace
            _ => fail!("unknown node in stmt: {:?}", node.as_ref()),
        }
    }

    fn expression(&mut self, node: impl AsRef<Node>) -> i32 {
        match &node.as_ref() {
            Node::Constant { val, .. } => {
                let r = self.next_reg();
                self.create(IRKind::Imm, reg_imm(r, *val as i32));
                r
            }

            Node::Conditional { cond, then, else_ } => {
                let x = self.next_label();
                let y = self.next_label();

                let r = self.expression(cond);
                self.create(IRKind::Unless, reg_imm(r, x));

                let r2 = self.expression(then);
                self.create(IRKind::Mov, reg_reg(r, r2));
                self.kill(r2);
                self.create(IRKind::Jmp, imm(y));
                self.create(IRKind::Label, imm(x));

                let r3 = self.expression(else_);
                self.create(IRKind::Mov, reg_reg(r, r3));
                self.kill(r2);
                self.create(IRKind::Label, imm(y));
                r
            }

            Node::Comma { lhs, rhs } => {
                let r = self.expression(lhs);
                self.kill(r);
                self.expression(rhs)
            }

            Node::Not { expr } => {
                let lhs = self.expression(expr);
                let rhs = self.next_reg();
                self.create(IRKind::Imm, reg_imm(rhs, 0));

                self.create(
                    IRKind::Comparison,
                    IRType::Cmp {
                        cmp: Comp::Equal,
                        dst: lhs,
                        src: rhs,
                    },
                );

                self.kill(rhs);
                lhs
            }

            Node::LogAnd { lhs, rhs } => {
                let x = self.next_label();

                let r1 = self.expression(lhs);
                self.create(IRKind::Unless, reg_imm(r1, x));

                let r2 = self.expression(rhs);
                self.create(IRKind::Mov, reg_reg(r1, r2));
                self.kill(r2);
                self.create(IRKind::Unless, reg_imm(r1, x));
                self.create(IRKind::Imm, reg_imm(r1, 1));
                self.create(IRKind::Label, imm(x));
                r1
            }

            Node::LogOr { lhs, rhs } => {
                let x = self.next_label();
                let y = self.next_label();

                let r1 = self.expression(lhs);
                self.create(IRKind::Unless, reg_imm(r1, x));
                self.create(IRKind::Imm, reg_imm(r1, 1));
                self.create(IRKind::Jmp, imm(y));
                self.create(IRKind::Label, imm(x));

                let r2 = self.expression(rhs);
                self.create(IRKind::Mov, reg_reg(r1, r2));
                self.kill(r2);
                self.create(IRKind::Unless, reg_imm(r1, y));
                self.create(IRKind::Imm, reg_imm(r1, 1));
                self.create(IRKind::Label, imm(y));
                r1
            }

            Node::Comparison { lhs, rhs, comp } => self.comparison(comp, lhs, rhs),

            Node::Call { name, args } => {
                let mut exprs = vec![];
                for arg in args {
                    exprs.push(self.expression(arg))
                }

                let r = self.next_reg();
                self.create(
                    IRKind::Call,
                    IRType::Call {
                        reg: r,
                        name: name.to_string(),
                        args: exprs.clone(),
                    },
                );

                for ex in exprs {
                    self.kill(ex);
                }
                r
            }

            Node::Assign { lhs, rhs } => {
                let l = lhs;

                let rhs = self.expression(rhs);
                let lhs = self.lvalue(lhs);

                self.store(&*l.get_type().as_ref().unwrap().borrow(), reg_reg(lhs, rhs));
                self.kill(rhs);
                lhs
            }

            Node::Add { lhs, rhs } | Node::Sub { lhs, rhs } => {
                if let Type::Ptr { .. } = &*lhs.get_type().as_ref().unwrap().borrow() {
                    let rhs = self.expression(rhs);
                    let size = types::size_of(
                        &*types::as_ptr(&Rc::clone(&lhs.get_type().as_ref().unwrap()))
                            .as_ref()
                            .unwrap()
                            .borrow(),
                    );

                    self.create(IRKind::Mul, reg_imm(rhs, size));

                    let lhs = self.expression(lhs);
                    let ir = match node.as_ref() {
                        Node::Add { .. } => IRKind::Add,
                        Node::Sub { .. } => IRKind::Sub,
                        _ => unreachable!(),
                    };
                    self.create(ir, reg_reg(lhs, rhs));
                    self.kill(rhs);
                    return lhs;
                }

                let ir = match node.as_ref() {
                    Node::Add { .. } => IRKind::Add,
                    Node::Sub { .. } => IRKind::Sub,
                    _ => unreachable!(),
                };
                self.binary(ir, lhs, rhs)
            }

            Node::Mul { lhs, rhs } => self.binary(IRKind::Mul, lhs, rhs),
            Node::Div { lhs, rhs } => self.binary(IRKind::Div, lhs, rhs),
            Node::Mod { lhs, rhs } => self.binary(IRKind::Mod, lhs, rhs),
            Node::Shr { lhs, rhs } => self.binary(IRKind::Shr, lhs, rhs),
            Node::Shl { lhs, rhs } => self.binary(IRKind::Shl, lhs, rhs),

            Node::Or { lhs, rhs } => self.binary(IRKind::Or, lhs, rhs),
            Node::Xor { lhs, rhs } => self.binary(IRKind::Xor, lhs, rhs),
            Node::And { lhs, rhs } => self.binary(IRKind::And, lhs, rhs),

            Node::Neg { expr } => {
                let r = self.expression(expr);
                self.create(IRKind::Neg, reg(r));
                r
            }

            Node::PreInc { expr } => self.pre_inc(expr, 1),
            Node::PreDec { expr } => self.pre_inc(expr, -1),
            Node::PostInc { expr } => self.post_inc(expr, 1),
            Node::PostDec { expr } => self.post_inc(expr, -1),

            Node::Addr { expr, .. } => self.lvalue(expr),

            Node::GVar { ty, .. } | Node::LVal { ty, .. } => {
                let r = self.lvalue(&node);
                self.load(&*ty.borrow(), reg_reg(r, r));
                r
            }

            Node::Dot { expr, .. } => {
                let r = self.lvalue(&node);
                self.load(&*expr.get_type().as_ref().unwrap().borrow(), reg_reg(r, r));
                r
            }

            Node::Deref { expr } => {
                let r = self.expression(expr);
                self.load(&*expr.get_type().as_ref().unwrap().borrow(), reg_reg(r, r));
                r
            }

            Node::Statement { stmt, ty: _ty } => {
                let l = self.ret_label;
                let r = self.ret_reg;

                self.ret_label = *self.label;
                *self.label += 1;

                let reg = self.next_reg();
                self.noop();
                self.ret_reg = reg;

                self.statement(stmt);
                self.create(IRKind::Label, imm(self.ret_label));

                self.ret_label = l;
                self.ret_reg = r;
                reg
            }

            // TODO make this return a Result so we can print out an instruction trace
            _ => fail!("unknown node in expr: {:?}", node.as_ref()),
        }
    }

    fn lvalue(&mut self, node: impl AsRef<Node>) -> i32 {
        let node = node.as_ref();
        if let Node::Deref { expr } = &node {
            return self.expression(expr);
        }

        if let Node::Dot { expr, offset, .. } = &node {
            let r = self.lvalue(expr);
            self.create(IRKind::Add, reg_imm(r, *offset));
            return r;
        }

        if let Node::LVal { offset, ty: _ty } = &node {
            let r = self.next_reg();
            self.create(IRKind::BpRel, reg_imm(r, *offset));
            return r;
        }

        if let Node::GVar { name, ty: _ty } = &node {
            let r = self.next_reg();
            self.create(IRKind::Label, reg_label(r, name.to_string()));
            return r;
        }

        unreachable!();
    }

    fn comparison(&mut self, cmp: &Comp, lhs: impl AsRef<Node>, rhs: impl AsRef<Node>) -> i32 {
        let r1 = self.expression(lhs);
        let r2 = self.expression(rhs);

        self.create(
            IRKind::Comparison,
            IRType::Cmp {
                cmp: cmp.clone(),
                dst: r1,
                src: r2,
            },
        );

        self.kill(r2);
        r1
    }

    fn binary(&mut self, ir: IRKind, lhs: impl AsRef<Node>, rhs: impl AsRef<Node>) -> i32 {
        let r1 = self.expression(lhs);
        let r2 = self.expression(rhs);

        self.push(IR {
            kind: ir,
            ty: reg_reg(r1, r2),
        });
        self.kill(r2);
        r1
    }

    fn pre_inc(&mut self, kind: &Kind, delta: i32) -> i32 {
        let addr = self.lvalue(kind);
        let val = self.next_reg();
        self.load(
            &*kind.get_type().as_ref().unwrap().borrow(),
            reg_reg(val, addr),
        );
        self.create(IRKind::Add, reg_imm(val, delta));
        self.storearg(
            &*kind.get_type().as_ref().unwrap().borrow(),
            reg_imm(addr, val),
        );
        self.kill(addr);
        val
    }

    fn post_inc(&mut self, kind: &Kind, delta: i32) -> i32 {
        let val = self.pre_inc(kind, delta);
        self.create(IRKind::Sub, reg_imm(val, delta));
        val
    }

    fn kill(&mut self, reg: i32) {
        self.push(IR {
            ty: IRType::Imm { val: reg },
            kind: IRKind::Kill,
        })
    }

    fn noop(&mut self) {
        self.push(IR {
            ty: IRType::Nop,
            kind: IRKind::Nop,
        });
    }

    fn store(&mut self, ty: &Type, irt: IRType) {
        let w = match ty {
            Type::Char => Width::W8,
            Type::Int => Width::W32,
            Type::Ptr { .. } | Type::Array { .. } | Type::Struct { .. } => Width::W64,
            _ => unreachable!(),
        };

        self.push(IR {
            ty: irt,
            kind: IRKind::Store(w),
        });
    }

    fn storearg(&mut self, ty: &Type, irt: IRType) {
        let w = match ty {
            Type::Char => Width::W8,
            Type::Int => Width::W32,
            Type::Ptr { .. } | Type::Array { .. } | Type::Struct { .. } => Width::W64,
            _ => unreachable!(),
        };

        self.push(IR {
            ty: irt,
            kind: IRKind::StoreArg(w),
        });
    }

    fn load(&mut self, ty: &Type, irt: IRType) {
        let w = match ty {
            Type::Char => Width::W8,
            Type::Int => Width::W32,
            Type::Ptr { .. } | Type::Array { .. } | Type::Struct { .. } => Width::W64,
            _ => unreachable!(),
        };

        self.push(IR {
            ty: irt,
            kind: IRKind::Load(w),
        });
    }

    fn next_reg(&mut self) -> i32 {
        let n = self.reg;
        self.reg += 1;
        n
    }

    fn next_label(&mut self) -> i32 {
        let n = *self.label;
        *self.label += 1;
        n
    }

    fn create(&mut self, kind: IRKind, ty: IRType) {
        self.push(IR { ty, kind })
    }

    fn push(&mut self, ir: IR) {
        self.inst.push(ir)
    }
}

// helpers to make IRTypes
#[inline]
fn reg_reg(dst: i32, src: i32) -> IRType {
    IRType::RegReg { dst, src }
}

#[inline]
fn reg_imm(reg: i32, val: i32) -> IRType {
    IRType::RegImm { reg, val }
}

#[inline]
fn reg(src: i32) -> IRType {
    IRType::Reg { src }
}

#[inline]
fn imm(val: i32) -> IRType {
    IRType::Imm { val }
}

#[inline]
fn reg_label(reg: i32, label: String) -> IRType {
    IRType::RegLabel { reg, label }
}

impl fmt::Display for IRType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ir::IRType::*;
        match self {
            RegReg { dst, src } => write!(f, "r{}, r{}", dst, src),
            RegImm { reg, val } => write!(f, "r{}, #{}", reg, val),
            RegLabel { reg, label } => write!(f, "r{}, {}", reg, label),
            Reg { src } => write!(f, "r{}", src),
            Imm { val } => write!(f, "#{}", val),
            Cmp { cmp, dst, src } => write!(f, "{} r{}, r{}", cmp, dst, src),
            Call { reg, name, args } => write!(f, "CALL r{} @ {}({:?})", reg, name, args),
            Nop => write!(f, "NOP"),
        }
    }
}

impl fmt::Display for IR {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ir::IRKind::*;

        let name = format!("{:?}", self.kind);
        let pos = match name.find('(') {
            Some(pos) => pos,
            None => name.len(),
        };
        let name = name[0..pos].to_uppercase();
        match &self.kind {
            Load(w) | Store(w) | StoreArg(w) => write!(f, "{}{} {}", &name, w, &self.ty),
            _ => write!(f, "{} {}", &name, &self.ty),
        }
    }
}
