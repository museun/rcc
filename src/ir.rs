use super::*;
use std::{
    fmt,
    ops::{Deref, DerefMut},
};

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
        cmp: Cmp,
        dst: i32,
        src: i32,
    },
    Nop,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Cmp {
    Gt, // same as lt assembly-wise
    Lt,
    Eq,
    NEq,
}

impl fmt::Display for Cmp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let w = match self {
            Cmp::Gt => ">",
            Cmp::Lt => "<",
            Cmp::Eq => "==",
            Cmp::NEq => "!=",
        };
        write!(f, "{}", w)
    }
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
    pub fn gen_ir(nodes: &[Node]) -> Vec<Function> {
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
                        inst: Vec::with_capacity(MAX_INST),
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

                        match arg.get_type() {
                            Type::Char => {
                                this.add(IR::StoreArg(Width::W8, reg_imm(i as i32, *offset)));
                            }
                            Type::Int => {
                                this.add(IR::StoreArg(Width::W32, reg_imm(i as i32, *offset)));
                            }
                            Type::Ptr { .. } | Type::Array { .. } | Type::Struct { .. } => {
                                this.add(IR::StoreArg(Width::W64, reg_imm(i as i32, *offset)));
                            }
                        }
                    }

                    this.gen_stmt(body);

                    let function = Function {
                        name: name.clone(),
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

    fn gen_stmt(&mut self, node: impl AsRef<Node>) {
        match &node.as_ref() {
            Node::Vardef { init, offset, .. } => {
                if !init.has_val() {
                    return;
                }

                let rhs = self.gen_expr(init);
                let lhs = self.next_reg();

                self.add(IR::BpRel(reg_imm(lhs, *offset)));

                match node.as_ref().get_type() {
                    Type::Char => {
                        self.add(IR::Store(Width::W8, reg_reg(lhs, rhs)));
                    }
                    Type::Int => {
                        self.add(IR::Store(Width::W32, reg_reg(lhs, rhs)));
                    }
                    Type::Ptr { .. } | Type::Array { .. } | Type::Struct { .. } => {
                        self.add(IR::Store(Width::W64, reg_reg(lhs, rhs)));
                    }
                }

                self.add(IR::Kill(reg(lhs)));
                self.add(IR::Kill(reg(rhs)));
            }

            Node::If { cond, body, else_ } => {
                let r = self.gen_expr(cond);
                let x = self.next_label();

                self.add(IR::Unless(reg_imm(r, x)));
                self.add(IR::Kill(reg(r)));

                self.gen_stmt(body);

                if !else_.has_val() {
                    self.add(IR::Label(imm(x)));
                    return;
                }

                let y = self.next_label();
                self.add(IR::Jmp(imm(y)));
                self.add(IR::Label(imm(x)));

                self.gen_stmt(else_);
                self.add(IR::Label(imm(y)));
            }

            Node::DoWhile { cond, body } => {
                let x = self.next_label();
                self.add(IR::Label(imm(x)));
                self.gen_stmt(body);

                let r = self.gen_expr(cond);
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
                    self.gen_stmt(init);
                }
                self.add(IR::Label(imm(x)));

                let r = self.gen_expr(cond);
                self.add(IR::Unless(reg_imm(r, y)));
                self.add(IR::Kill(reg(r)));
                self.gen_stmt(body);

                if step.has_val() {
                    self.gen_stmt(step);
                }
                self.add(IR::Jmp(imm(x)));
                self.add(IR::Label(imm(y)));
            }

            Node::Return { expr } => {
                let r = self.gen_expr(expr);
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
                let r = self.gen_expr(expr);
                self.add(IR::Kill(reg(r)));
            }

            Node::Compound { ref stmts } => {
                for stmt in stmts {
                    self.gen_stmt(stmt)
                }
            }
            Node::Noop {} => {}
            // TODO make this return a Result so we can print out an instruction trace
            _ => fail!("unknown node in stmt: {:?}", node.as_ref()),
        }
    }

    fn gen_expr(&mut self, node: impl AsRef<Node>) -> i32 {
        match &node.as_ref() {
            Node::Constant { val, .. } => {
                let r = self.next_reg();
                self.add(IR::Imm(reg_imm(r, *val as i32)));
                r
            }

            Node::LogAnd { lhs, rhs } => {
                let x = self.next_label();

                let r1 = self.gen_expr(lhs);
                self.add(IR::Unless(reg_imm(r1, x)));

                let r2 = self.gen_expr(rhs);
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

                let r1 = self.gen_expr(lhs);
                self.add(IR::Unless(reg_imm(r1, x)));
                self.add(IR::Imm(reg_imm(r1, 1)));
                self.add(IR::Jmp(imm(y)));
                self.add(IR::Label(imm(x)));

                let r2 = self.gen_expr(rhs);
                self.add(IR::Mov(reg_reg(r1, r2)));
                self.add(IR::Kill(reg(r2)));
                self.add(IR::Unless(reg_imm(r1, y)));
                self.add(IR::Imm(reg_imm(r1, 1)));
                self.add(IR::Label(imm(y)));
                r1
            }

            Node::Equals { lhs, rhs } => self.gen_cmp(Cmp::Eq, lhs, rhs),

            Node::NEquals { lhs, rhs } => self.gen_cmp(Cmp::NEq, lhs, rhs),

            Node::Comparison { lhs, rhs, comp } => self.gen_cmp(
                match comp {
                    Comp::Lt => Cmp::Lt,
                    Comp::Gt => Cmp::Gt,
                },
                lhs,
                rhs,
            ),

            Node::Call { name, args } => {
                let mut exprs = vec![];
                for arg in args {
                    exprs.push(self.gen_expr(arg))
                }

                let r = self.next_reg();
                self.add(IR::Call(IRType::Call {
                    reg: r,
                    name: name.clone(),
                    args: exprs.clone(),
                }));

                for ex in exprs {
                    self.add(IR::Kill(reg(ex)));
                }
                r
            }

            Node::Assign { lhs, rhs } => {
                let l = lhs;

                let rhs = self.gen_expr(rhs);
                let lhs = self.gen_lval(lhs);

                match l.get_type() {
                    Type::Char => self.add(IR::Store(Width::W8, reg_reg(lhs, rhs))),
                    Type::Int => self.add(IR::Store(Width::W32, reg_reg(lhs, rhs))),
                    Type::Ptr { .. } | Type::Array { .. } | Type::Struct { .. } => {
                        self.add(IR::Store(Width::W64, reg_reg(lhs, rhs)))
                    }
                };

                self.add(IR::Kill(reg(rhs)));
                lhs
            }

            Node::Add { lhs, rhs, ty: _ty } | Node::Sub { lhs, rhs, ty: _ty } => {
                if let Type::Ptr { .. } = lhs.get_type() {
                    let rhs = self.gen_expr(rhs);
                    let r = self.next_reg();
                    self.add(IR::Imm(reg_imm(
                        r,
                        lhs.get_type()
                            .as_ptr()
                            .expect("add lhs to be a pointer")
                            .size(),
                    )));
                    self.add(IR::Mul(reg_reg(rhs, r)));
                    self.add(IR::Kill(reg(r)));

                    let lhs = self.gen_expr(lhs);
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
                self.gen_binops(ir, lhs, rhs)
            }

            Node::Mul { lhs, rhs, ty: _ty } => self.gen_binops(IR::Mul(IRType::Nop), lhs, rhs),

            Node::Div { lhs, rhs, ty: _ty } => self.gen_binops(IR::Div(IRType::Nop), lhs, rhs),

            Node::Addr { expr, .. } => self.gen_lval(expr),

            Node::GVar { ty, .. } | Node::LVal { ty, .. } => {
                let r = self.gen_lval(&node);
                self.add(load_instruction(ty, reg_reg(r, r)));
                r
            }

            Node::Dot { expr, .. } => {
                let r = self.gen_lval(&node);
                self.add(load_instruction(expr.get_type(), reg_reg(r, r)));
                r
            }

            Node::Deref { expr } => {
                let r = self.gen_expr(expr);
                self.add(load_instruction(
                    expr.get_type().as_ptr().expect("deref to be a pointer"),
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

                self.gen_stmt(stmt);
                self.add(IR::Label(imm(self.ret_label)));

                self.ret_label = l;
                self.ret_reg = r;
                reg
            }

            // TODO make this return a Result so we can print out an instruction trace
            _ => fail!("unknown node in expr: {:?}", node.as_ref()),
        }
    }

    fn gen_lval(&mut self, node: impl AsRef<Node>) -> i32 {
        let node = node.as_ref();
        if let Node::Deref { expr } = &node {
            return self.gen_expr(expr);
        }

        if let Node::Dot { expr, offset, .. } = &node {
            let r1 = self.gen_lval(expr);
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
            self.add(IR::Label(reg_label(r, name.clone())));
            return r;
        }

        unreachable!();
    }

    fn gen_cmp(&mut self, cmp: Cmp, lhs: impl AsRef<Node>, rhs: impl AsRef<Node>) -> i32 {
        let r1 = self.gen_expr(lhs);
        let r2 = self.gen_expr(rhs);

        self.add(IR::Comparison(IRType::Cmp {
            cmp,
            dst: r1,
            src: r2,
        }));
        self.add(IR::Kill(reg(r2)));
        r1
    }

    fn gen_binops(&mut self, mut ir: IR, lhs: impl AsRef<Node>, rhs: impl AsRef<Node>) -> i32 {
        let r1 = self.gen_expr(lhs);
        let r2 = self.gen_expr(rhs);
        eprintln!("a ir: {:#?}", ir);
        *ir = reg_reg(r1, r2);
        eprintln!("b ir: {:#?}", ir);
        self.add(ir);
        self.add(IR::Kill(reg(r2)));
        r1
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
        use IR::*;
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
            | Comparison(ty)
            | Kill(ty)
            | Nop(ty)
            | Call(ty) => ty,
        }
    }
}

impl DerefMut for IR {
    fn deref_mut(&mut self) -> &mut Self::Target {
        use IR::*;
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
            | Comparison(ty)
            | Kill(ty)
            | Nop(ty)
            | Call(ty) => ty,
        }
    }
}

impl fmt::Display for IRType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use IRType::*;
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
        use IR::*;

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
            Comparison(ty) => write!(f, "CMP {}", ty),
            Kill(ty) => write!(f, "KILL {}", ty),
            Nop(ty) => write!(f, "NOP {}", ty),
            Call(ty) => write!(f, "CALL {}", ty),
        }
    }
}
