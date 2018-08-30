use super::*;
use node::{Comp, Node};
use types::{Type, Var};

use std::fmt;
use std::ops::{Deref, DerefMut};

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

#[derive(Debug, Clone, PartialEq)]
pub enum IR {
    Imm(IRType),    // reg->imm
    Mov(IRType),    // reg->reg
    Return(IRType), // reg

    Load(Width, IRType),     // reg->reg
    Store(Width, IRType),    // reg->reg
    StoreArg(Width, IRType), // reg->reg

    Unless(IRType), // reg->imm
    Label(IRType),  // imm OR reg_label
    Jmp(IRType),    // imm
    If(IRType),     // reg->imm

    BpRel(IRType), // reg->imm

    Add(IRType), // reg->reg
    Sub(IRType), // reg->reg
    Mul(IRType), // reg->reg
    Div(IRType), // reg->reg
    Mod(IRType), // reg->reg

    Shr(IRType), // reg->reg
    Shl(IRType), // reg->reg

    Or(IRType),  // reg->reg
    Xor(IRType), // reg->reg
    And(IRType), // reg->reg

    Neg(IRType), // reg

    Comparison(IRType), // cmp, reg->reg

    Kill(IRType), // reg
    Nop(IRType),  // nothing
    Call(IRType), // call name, [args]
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

                        match &*arg.get_type().as_ref().unwrap().borrow() {
                            Type::Char => {
                                this.add(IR::StoreArg(Width::W8, reg_imm(i as i32, *offset)));
                            }
                            Type::Int => {
                                this.add(IR::StoreArg(Width::W32, reg_imm(i as i32, *offset)));
                            }
                            Type::Ptr { .. } | Type::Array { .. } | Type::Struct { .. } => {
                                this.add(IR::StoreArg(Width::W64, reg_imm(i as i32, *offset)));
                            }
                            _ => unreachable!(),
                        }
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

                self.add(IR::BpRel(reg_imm(lhs, *offset)));

                match &*node.as_ref().get_type().as_ref().unwrap().borrow() {
                    Type::Char => {
                        self.add(IR::Store(Width::W8, reg_reg(lhs, rhs)));
                    }
                    Type::Int => {
                        self.add(IR::Store(Width::W32, reg_reg(lhs, rhs)));
                    }
                    Type::Ptr { .. } | Type::Array { .. } | Type::Struct { .. } => {
                        self.add(IR::Store(Width::W64, reg_reg(lhs, rhs)));
                    }
                    _ => unreachable!(),
                }

                self.add(IR::Kill(reg(lhs)));
                self.add(IR::Kill(reg(rhs)));
            }

            Node::If { cond, body, else_ } => {
                let r = self.expression(cond);
                let x = self.next_label();

                self.add(IR::Unless(reg_imm(r, x)));
                self.add(IR::Kill(reg(r)));

                self.statement(body);

                if !else_.has_val() {
                    self.add(IR::Label(imm(x)));
                    return;
                }

                let y = self.next_label();
                self.add(IR::Jmp(imm(y)));
                self.add(IR::Label(imm(x)));

                self.statement(else_);
                self.add(IR::Label(imm(y)));
            }

            Node::DoWhile { cond, body } => {
                let x = self.next_label();
                self.add(IR::Label(imm(x)));
                self.statement(body);

                let r = self.expression(cond);
                self.add(IR::If(reg_imm(r, x)));
                self.add(IR::Kill(reg(r)));
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
                self.add(IR::Label(imm(x)));

                let r = self.expression(cond);
                self.add(IR::Unless(reg_imm(r, y)));
                self.add(IR::Kill(reg(r)));
                self.statement(body);

                if step.has_val() {
                    self.statement(step);
                }
                self.add(IR::Jmp(imm(x)));
                self.add(IR::Label(imm(y)));
            }

            Node::Return { expr } => {
                let r = self.expression(expr);
                if self.ret_label != 0 {
                    self.add(IR::Mov(reg_reg(self.ret_reg, r)));
                    self.add(IR::Kill(reg(r)));
                    self.add(IR::Jmp(imm(self.ret_label)));
                    return;
                }
                self.add(IR::Return(reg(r)));
                self.add(IR::Kill(reg(r)));
            }

            Node::Expression { expr } => {
                let r = self.expression(expr);
                self.add(IR::Kill(reg(r)));
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
                self.add(IR::Imm(reg_imm(r, *val as i32)));
                r
            }

            Node::Conditional { cond, then, else_ } => {
                let x = self.next_label();
                let y = self.next_label();

                let r = self.expression(cond);
                self.add(IR::Unless(reg_imm(r, x)));

                let r2 = self.expression(then);
                self.add(IR::Mov(reg_reg(r, r2)));
                self.add(IR::Kill(reg(r2)));
                self.add(IR::Jmp(imm(y)));
                self.add(IR::Label(imm(x)));

                let r3 = self.expression(else_);
                self.add(IR::Mov(reg_reg(r, r3)));
                self.add(IR::Kill(reg(r2)));
                self.add(IR::Label(imm(y)));
                r
            }

            Node::Comma { lhs, rhs } => {
                let r = self.expression(lhs);
                self.add(IR::Kill(reg(r)));
                self.expression(rhs)
            }

            Node::Not { expr } => {
                let lhs = self.expression(expr);
                let rhs = self.next_reg();
                self.add(IR::Imm(reg_imm(rhs, 0)));

                self.add(IR::Comparison(IRType::Cmp {
                    cmp: Comp::Eq,
                    dst: lhs,
                    src: rhs,
                }));

                self.add(IR::Kill(reg(rhs)));
                lhs
            }

            Node::LogAnd { lhs, rhs } => {
                let x = self.next_label();

                let r1 = self.expression(lhs);
                self.add(IR::Unless(reg_imm(r1, x)));

                let r2 = self.expression(rhs);
                self.add(IR::Mov(reg_reg(r1, r2)));
                self.add(IR::Kill(reg(r2)));
                self.add(IR::Unless(reg_imm(r1, x)));
                self.add(IR::Imm(reg_imm(r1, 1)));
                self.add(IR::Label(imm(x)));
                r1
            }

            Node::LogOr { lhs, rhs } => {
                let x = self.next_label();
                let y = self.next_label();

                let r1 = self.expression(lhs);
                self.add(IR::Unless(reg_imm(r1, x)));
                self.add(IR::Imm(reg_imm(r1, 1)));
                self.add(IR::Jmp(imm(y)));
                self.add(IR::Label(imm(x)));

                let r2 = self.expression(rhs);
                self.add(IR::Mov(reg_reg(r1, r2)));
                self.add(IR::Kill(reg(r2)));
                self.add(IR::Unless(reg_imm(r1, y)));
                self.add(IR::Imm(reg_imm(r1, 1)));
                self.add(IR::Label(imm(y)));
                r1
            }

            Node::Comparison { lhs, rhs, comp } => self.comparison(comp, lhs, rhs),

            Node::Call { name, args } => {
                let mut exprs = vec![];
                for arg in args {
                    exprs.push(self.expression(arg))
                }

                let r = self.next_reg();
                self.add(IR::Call(IRType::Call {
                    reg: r,
                    name: name.to_string(),
                    args: exprs.clone(),
                }));

                for ex in exprs {
                    self.add(IR::Kill(reg(ex)));
                }
                r
            }

            Node::Assign { lhs, rhs } => {
                let l = lhs;

                let rhs = self.expression(rhs);
                let lhs = self.lvalue(lhs);

                match &*l.get_type().as_ref().unwrap().borrow() {
                    Type::Char => self.add(IR::Store(Width::W8, reg_reg(lhs, rhs))),
                    Type::Int => self.add(IR::Store(Width::W32, reg_reg(lhs, rhs))),
                    Type::Ptr { .. } | Type::Array { .. } | Type::Struct { .. } => {
                        self.add(IR::Store(Width::W64, reg_reg(lhs, rhs)))
                    }
                    _ => unreachable!(),
                };

                self.add(IR::Kill(reg(rhs)));
                lhs
            }

            Node::Or { lhs, rhs } => self.binary(IR::Or(IRType::Nop), lhs, rhs),
            Node::Xor { lhs, rhs } => self.binary(IR::Xor(IRType::Nop), lhs, rhs),
            Node::And { lhs, rhs } => self.binary(IR::And(IRType::Nop), lhs, rhs),

            Node::Add { lhs, rhs } | Node::Sub { lhs, rhs } => {
                if let Type::Ptr { .. } = &*lhs.get_type().as_ref().unwrap().borrow() {
                    let rhs = self.expression(rhs);
                    let r = self.next_reg();
                    self.add(IR::Imm(reg_imm(
                        r,
                        types::size_of(
                            &*types::as_ptr(Rc::clone(&lhs.get_type().as_ref().unwrap()))
                                .as_ref()
                                .unwrap()
                                .borrow(),
                        ),
                    )));
                    self.add(IR::Mul(reg_reg(rhs, r)));
                    self.add(IR::Kill(reg(r)));

                    let lhs = self.expression(lhs);
                    let ir = match node.as_ref() {
                        Node::Add { .. } => IR::Add(reg_reg(lhs, rhs)),
                        Node::Sub { .. } => IR::Sub(reg_reg(lhs, rhs)),
                        _ => unreachable!(),
                    };
                    self.add(ir);
                    self.add(IR::Kill(reg(rhs)));
                    return lhs;
                }

                let ir = match node.as_ref() {
                    Node::Add { .. } => IR::Add(IRType::Nop),
                    Node::Sub { .. } => IR::Sub(IRType::Nop),
                    _ => unreachable!(),
                };
                self.binary(ir, lhs, rhs)
            }

            Node::Mul { lhs, rhs } => self.binary(IR::Mul(IRType::Nop), lhs, rhs),

            Node::Div { lhs, rhs } => self.binary(IR::Div(IRType::Nop), lhs, rhs),

            Node::Mod { lhs, rhs } => self.binary(IR::Mod(IRType::Nop), lhs, rhs),

            Node::Shr { lhs, rhs } => self.binary(IR::Shr(IRType::Nop), lhs, rhs),

            Node::Shl { lhs, rhs } => self.binary(IR::Shl(IRType::Nop), lhs, rhs),

            Node::Neg { expr } => {
                let r = self.expression(expr);
                self.add(IR::Neg(reg(r)));
                r
            }

            Node::PreInc { expr } => {
                let lhs = self.lvalue(expr);
                self.pre_inc(lhs, &*expr.get_type().as_ref().unwrap().borrow(), 1)
            }
            Node::PreDec { expr } => {
                let lhs = self.lvalue(expr);
                self.pre_inc(lhs, &*expr.get_type().as_ref().unwrap().borrow(), -1)
            }
            Node::PostInc { expr } => {
                let lhs = self.lvalue(expr);
                self.post_inc(lhs, &*expr.get_type().as_ref().unwrap().borrow(), 1)
            }
            Node::PostDec { expr } => {
                let lhs = self.lvalue(expr);
                self.post_inc(lhs, &*expr.get_type().as_ref().unwrap().borrow(), -1)
            }

            Node::Addr { expr, .. } => self.lvalue(expr),

            Node::GVar { ty, .. } | Node::LVal { ty, .. } => {
                let r = self.lvalue(&node);
                self.add(load_instruction(&*ty.borrow(), reg_reg(r, r)));
                r
            }

            Node::Dot { expr, .. } => {
                let r = self.lvalue(&node);
                self.add(load_instruction(
                    &*expr.get_type().as_ref().unwrap().borrow(),
                    reg_reg(r, r),
                ));
                r
            }

            Node::Deref { expr } => {
                let r = self.expression(expr);
                self.add(load_instruction(
                    &*expr.get_type().as_ref().unwrap().borrow(),
                    reg_reg(r, r),
                ));
                r
            }

            Node::Statement { stmt, ty: _ty } => {
                let l = self.ret_label;
                let r = self.ret_reg;
                self.ret_label = *self.label;
                *self.label += 1;
                let reg = self.next_reg();
                self.add(IR::Nop(IRType::Nop));

                self.ret_reg = reg;

                self.statement(stmt);
                self.add(IR::Label(imm(self.ret_label)));

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
            let r1 = self.lvalue(expr);
            let r2 = self.next_reg();
            self.add(IR::Imm(reg_imm(r2, *offset)));
            self.add(IR::Add(reg_reg(r1, r2)));
            self.add(IR::Kill(reg(r2)));
            return r1;
        }

        if let Node::LVal { offset, ty: _ty } = &node {
            let r = self.next_reg();
            self.add(IR::BpRel(reg_imm(r, *offset)));
            return r;
        }

        if let Node::GVar { name, ty: _ty } = &node {
            let r = self.next_reg();
            self.add(IR::Label(reg_label(r, name.to_string())));
            return r;
        }

        unreachable!();
    }

    fn comparison(&mut self, cmp: &Comp, lhs: impl AsRef<Node>, rhs: impl AsRef<Node>) -> i32 {
        let r1 = self.expression(lhs);
        let r2 = self.expression(rhs);

        self.add(IR::Comparison(IRType::Cmp {
            cmp: cmp.clone(),
            dst: r1,
            src: r2,
        }));
        self.add(IR::Kill(reg(r2)));
        r1
    }

    fn binary(&mut self, mut ir: IR, lhs: impl AsRef<Node>, rhs: impl AsRef<Node>) -> i32 {
        let r1 = self.expression(lhs);
        let r2 = self.expression(rhs);
        *ir = reg_reg(r1, r2);
        self.add(ir);
        self.add(IR::Kill(reg(r2)));
        r1
    }

    fn pre_inc(&mut self, addr: i32, ty: &Type, delta: i32) -> i32 {
        let val = self.next_reg();
        self.add(load_instruction(ty, reg_reg(val, addr)));
        let imm = self.next_reg();

        self.add(IR::Imm(reg_imm(imm, delta)));
        self.add(IR::Add(reg_reg(val, imm)));
        self.add(IR::Kill(reg(imm)));
        self.add(IR::StoreArg(
            match ty {
                Type::Char => Width::W8,
                Type::Int => Width::W32,
                Type::Ptr { .. } | Type::Array { .. } | Type::Struct { .. } => Width::W64,
                _ => unreachable!(),
            },
            reg_imm(addr, val),
        ));
        self.add(IR::Kill(reg(addr)));

        val
    }

    fn post_inc(&mut self, addr: i32, ty: &Type, delta: i32) -> i32 {
        let val = self.next_reg();
        self.add(load_instruction(ty, reg_reg(val, addr)));
        let imm = self.next_reg();

        self.add(IR::Imm(reg_imm(imm, delta)));
        self.add(IR::Add(reg_reg(val, imm)));
        self.add(IR::StoreArg(
            match ty {
                Type::Char => Width::W8,
                Type::Int => Width::W32,
                Type::Ptr { .. } | Type::Array { .. } | Type::Struct { .. } => Width::W64,
                _ => unreachable!(),
            },
            reg_imm(addr, val),
        ));
        self.add(IR::Kill(reg(addr)));
        self.add(IR::Sub(reg_reg(val, imm)));
        self.add(IR::Kill(reg(imm)));

        val
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

    fn add(&mut self, ir: IR) {
        self.inst.push(ir)
    }
}

// helpers to make IRTypes

#[inline]
fn load_instruction(ty: &Type, irt: IRType) -> IR {
    match ty {
        Type::Char => IR::Load(Width::W8, irt),
        Type::Int => IR::Load(Width::W32, irt),
        Type::Ptr { .. } | Type::Array { .. } | Type::Struct { .. } => IR::Load(Width::W64, irt),
        _ => unreachable!(),
    }
}

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

// TODO: this stuff needs to be rewritten
impl Deref for IR {
    type Target = IRType;

    fn deref(&self) -> &Self::Target {
        use ir::IR::*;
        match self {
            Imm(ty)
            | Mov(ty)
            | Return(ty)
            | Load(_, ty)
            | Store(_, ty)
            | StoreArg(_, ty)
            | BpRel(ty)
            | Unless(ty)
            | Label(ty)
            | Jmp(ty)
            | If(ty)
            | Add(ty)
            | Sub(ty)
            | Mul(ty)
            | Div(ty)
            | Shr(ty)
            | Shl(ty)
            | Mod(ty)
            | Or(ty)
            | Xor(ty)
            | And(ty)
            | Neg(ty)
            | Comparison(ty)
            | Kill(ty)
            | Nop(ty)
            | Call(ty) => ty,
        }
    }
}

impl DerefMut for IR {
    fn deref_mut(&mut self) -> &mut Self::Target {
        use ir::IR::*;
        match self {
            Imm(ty)
            | Mov(ty)
            | Return(ty)
            | Load(_, ty)
            | Store(_, ty)
            | StoreArg(_, ty)
            | BpRel(ty)
            | Unless(ty)
            | Label(ty)
            | If(ty)
            | Jmp(ty)
            | Add(ty)
            | Sub(ty)
            | Mul(ty)
            | Div(ty)
            | Mod(ty)
            | Shr(ty)
            | Shl(ty)
            | Or(ty)
            | Xor(ty)
            | And(ty)
            | Neg(ty)
            | Comparison(ty)
            | Kill(ty)
            | Nop(ty)
            | Call(ty) => ty,
        }
    }
}

impl fmt::Display for IRType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ir::IRType::*;
        match self {
            RegReg { dst, src } => write!(f, "r{}, r{}", dst, src),
            RegImm { reg, val } => write!(f, "r{}, {}", reg, val),
            RegLabel { reg, label } => write!(f, "r{}, {}", reg, label),
            Reg { src } => write!(f, "r{}", src),
            Imm { val } => write!(f, "{}", val),
            Cmp { cmp, dst, src } => write!(f, "{} r{}, r{}", cmp, dst, src),
            Call { reg, name, args } => write!(f, "CALL r{} @ {}({:?})", reg, name, args),
            Nop => write!(f, "NOP"),
        }
    }
}

impl fmt::Display for IR {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ir::IR::*;

        match self {
            Imm(ty) => write!(f, "IMM {}", ty),
            Mov(ty) => write!(f, "MOV {}", ty),
            Return(ty) => write!(f, "RETURN {} ", ty),
            Load(w, ty) => write!(f, "LOAD {} {}", w, ty),
            Store(w, ty) => write!(f, "STORE {} {}", w, ty),
            StoreArg(w, ty) => write!(f, "STOREARG {} {}", w, ty),
            BpRel(ty) => write!(f, "BPREL {}", ty),
            Unless(ty) => write!(f, "UNLESS {}", ty),
            Label(ty) => write!(f, "LABEL {}", ty),
            Jmp(ty) => write!(f, "JMP {}", ty),
            If(ty) => write!(f, "IF {}", ty),
            Add(ty) => write!(f, "ADD {}", ty),
            Sub(ty) => write!(f, "SUB {}", ty),
            Mul(ty) => write!(f, "MUL {}", ty),
            Div(ty) => write!(f, "DIV {}", ty),
            Shr(ty) => write!(f, "SHR {}", ty),
            Shl(ty) => write!(f, "SHL {}", ty),
            Mod(ty) => write!(f, "MOD {}", ty),
            Or(ty) => write!(f, "OR {}", ty),
            Xor(ty) => write!(f, "XOR {}", ty),
            And(ty) => write!(f, "AND {}", ty),
            Neg(ty) => write!(f, "NEG {}", ty),
            Comparison(ty) => write!(f, "CMP {}", ty),
            Kill(ty) => write!(f, "KILL {}", ty),
            Nop(ty) => write!(f, "NOP {}", ty),
            Call(ty) => write!(f, "CALL {}", ty),
        }
    }
}
