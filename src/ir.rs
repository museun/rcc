use super::*;
use std::{
    collections::HashMap,
    fmt,
    ops::{Deref, DerefMut},
};

#[derive(Clone, PartialEq)]
pub enum IRType {
    RegReg {
        dst: i32,
        src: i32,
    },
    RegImm {
        reg: i32,
        val: i32,
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
    Nop,
}

#[derive(Clone, PartialEq)]
pub enum IR {
    Imm(IRType),      // reg->imm
    Mov(IRType),      // reg->reg
    Return(IRType),   // reg
    Load(IRType),     // reg->reg
    Store(IRType),    // reg->reg
    Unless(IRType),   // reg->imm
    Label(IRType),    // imm
    Jmp(IRType),      // imm
    Add(IRType),      // reg->reg
    Sub(IRType),      // reg->reg
    Mul(IRType),      // reg->reg
    Div(IRType),      // reg->reg
    LessThan(IRType), // reg->reg
    Kill(IRType),     // reg
    Nop(IRType),      // nothing
    Call(IRType),     // call name, [args]
    SaveArgs(IRType), // args + stack offset
}

#[derive(Debug)]
pub struct Function {
    pub(crate) name: String,
    pub(crate) stacksize: i32,
    pub(crate) ir: Vec<IR>,
}

#[derive(Debug)]
pub struct Generate {
    inst: Vec<IR>,
    label: i32,
}

impl Generate {
    pub fn gen_ir(nodes: &[Node]) -> Vec<Function> {
        let mut out = vec![];
        for node in nodes {
            match node {
                Node::Func {
                    name,
                    body,
                    args,
                    stacksize,
                } => {
                    let mut this = Self {
                        // TODO be smarter about this
                        inst: Vec::with_capacity(MAX_INST),
                        label: 0,
                    };

                    this.add(IR::SaveArgs(imm(args.len() as i32)));
                    this.gen_stmt(body.as_ref().unwrap());

                    let function = Function {
                        name: name.clone(),
                        stacksize: *stacksize,
                        ir: this.inst,
                    };
                    out.push(function);
                }
                node => fail!("expected a function node, got: {:?} ", node),
            }
        }
        out
    }

    fn gen_stmt(&mut self, node: &Node) {
        match node {
            // TODO or is this the size?
            Node::Vardef { name, init, offset } => {
                if init.is_none() {
                    return;
                }

                let rhs = self.gen_expr(init.as_ref().unwrap());
                let lhs = self.next_reg();
                self.add(IR::Mov(reg_reg(lhs, 0)));
                self.add(IR::Sub(reg_imm(lhs, *offset)));
                self.add(IR::Store(reg_reg(lhs, rhs)));
                self.add(IR::Kill(reg(lhs)));
                self.add(IR::Kill(reg(rhs)));
            }

            Node::If { cond, body, else_ } => {
                let r = self.gen_expr(cond.as_ref().unwrap());
                let x = self.next_label();

                self.add(IR::Unless(reg_imm(r, x)));
                self.add(IR::Kill(reg(r)));

                self.gen_stmt(body.as_ref().unwrap());

                if else_.is_none() {
                    self.add(IR::Label(imm(x)));
                    return;
                }

                let y = self.next_label();
                self.add(IR::Jmp(imm(y)));
                self.add(IR::Label(imm(x)));

                self.gen_stmt(else_.as_ref().unwrap());
                self.add(IR::Label(imm(y)));
            }

            Node::For {
                init,
                cond,
                step,
                body,
            } => {
                let x = self.next_label();
                let y = self.next_label();

                self.gen_stmt(init.as_ref().unwrap());
                self.add(IR::Label(imm(x)));

                let r = self.gen_expr(cond.as_ref().unwrap());
                self.add(IR::Unless(reg_imm(r, y)));
                self.add(IR::Kill(reg(r)));
                self.gen_stmt(body.as_ref().unwrap());

                let n = self.gen_expr(step.as_ref().unwrap());
                self.add(IR::Kill(reg(n)));
                self.add(IR::Jmp(imm(x)));
                self.add(IR::Label(imm(y)));
            }

            Node::Return { expr } => {
                let r = self.gen_expr(expr.as_ref().unwrap());
                self.add(IR::Return(reg(r)));
                self.add(IR::Kill(reg(r)));
            }

            Node::Statement { expr } => {
                let r = self.gen_expr(expr.as_ref().unwrap());
                self.add(IR::Kill(reg(r)));
            }

            Node::Compound { stmts } => {
                for stmt in stmts {
                    self.gen_stmt(&stmt)
                }
            }
            // TODO make this return a Result so we can print out an instruction trace
            _ => fail!("unknown node in stmt: {:?}", node),
        }
    }

    fn gen_expr(&mut self, node: &Node) -> i32 {
        match &node {
            Node::Constant { val } => {
                let r = self.next_reg();
                self.add(IR::Add(reg_imm(r, *val as i32)));
                r
            }

            Node::LogAnd { lhs, rhs } => {
                let x = self.next_label();

                let r1 = self.gen_expr(lhs.as_ref().unwrap());
                self.add(IR::Unless(reg_imm(r1, x)));

                let r2 = self.gen_expr(rhs.as_ref().unwrap());
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

                let r1 = self.gen_expr(lhs.as_ref().unwrap());
                self.add(IR::Unless(reg_imm(r1, x)));
                self.add(IR::Imm(reg_imm(r1, 1)));
                self.add(IR::Jmp(imm(y)));
                self.add(IR::Label(imm(x)));

                let r2 = self.gen_expr(rhs.as_ref().unwrap());
                self.add(IR::Mov(reg_reg(r1, r2)));
                self.add(IR::Kill(reg(r2)));
                self.add(IR::Unless(reg_imm(r1, y)));
                self.add(IR::Imm(reg_imm(r1, 1)));
                self.add(IR::Label(imm(y)));

                r1
            }

            Node::LVal { .. } => {
                let r = self.gen_lval(node);
                self.add(IR::Load(reg_reg(r, r)));
                r
            }

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

            Node::LessThan { lhs, rhs } => self.gen_binops(
                IR::LessThan(IRType::Nop),
                lhs.as_ref().unwrap(),
                rhs.as_ref().unwrap(),
            ),

            Node::Assign { lhs, rhs } => {
                let rhs = self.gen_expr(rhs.as_ref().unwrap());
                let lhs = self.gen_lval(lhs.as_ref().unwrap());
                self.add(IR::Store(reg_reg(lhs, rhs)));
                self.add(IR::Kill(reg(rhs)));
                lhs
            }

            // TODO use gen_binops for this
            Node::Expression { lhs, rhs, tok } => {
                let lhs = self.gen_expr(lhs.as_ref().unwrap());
                let rhs = self.gen_expr(rhs.as_ref().unwrap());
                let ty = reg_reg(lhs, rhs);
                let ir = match tok {
                    Token::Add => IR::Add(ty),
                    Token::Sub => IR::Sub(ty),
                    Token::Mul => IR::Mul(ty),
                    Token::Div => IR::Div(ty),
                    _ => fail!("invalid node type"),
                };
                self.add(ir);
                self.add(IR::Kill(reg(rhs)));
                lhs
            }
            // TODO make this return a Result so we can print out an instruction trace
            _ => fail!("unknown node in expr: {:?}", node),
        }
    }

    fn gen_binops(&mut self, mut ir: IR, lhs: &Node, rhs: &Node) -> i32 {
        let r1 = self.gen_expr(lhs);
        let r2 = self.gen_expr(rhs);
        *ir = reg_reg(r1, r2);
        self.add(ir); // ??
        self.add(IR::Kill(reg(r2)));
        r1
    }

    fn gen_lval(&mut self, node: &Node) -> i32 {
        if let Node::LVal { offset } = &node {
            let r = self.next_reg();
            self.add(IR::Mov(reg_reg(r, 0)));
            self.add(IR::Sub(reg_imm(r, *offset)));
            return r;
        }

        fail!("not an lvalue: {:?}", node);
    }

    fn next_reg(&self) -> i32 {
        (self.inst.len() + 1) as i32
    }

    fn next_label(&mut self) -> i32 {
        let n = self.label;
        self.label += 1;
        n
    }

    fn add(&mut self, ir: IR) {
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

// TODO: this stuff needs to be rewritten
impl Deref for IR {
    type Target = IRType;

    fn deref(&self) -> &Self::Target {
        use IR::*;
        match self {
            Imm(ty) | Mov(ty) | Return(ty) | Load(ty) | Store(ty) | Unless(ty) | Label(ty)
            | Jmp(ty) | Add(ty) | Sub(ty) | Mul(ty) | Div(ty) | LessThan(ty) | Kill(ty)
            | Nop(ty) | Call(ty) | SaveArgs(ty) => ty,
        }
    }
}

impl DerefMut for IR {
    fn deref_mut(&mut self) -> &mut Self::Target {
        use IR::*;
        match self {
            Imm(ty) | Mov(ty) | Return(ty) | Load(ty) | Store(ty) | Unless(ty) | Label(ty)
            | Jmp(ty) | Add(ty) | Sub(ty) | Mul(ty) | Div(ty) | LessThan(ty) | Kill(ty)
            | Nop(ty) | Call(ty) | SaveArgs(ty) => ty,
        }
    }
}

impl fmt::Debug for IRType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use IRType::*;
        match self {
            RegReg { dst, src } => write!(f, "RegReg {{ dst: {:?}, src: {:?} }}", dst, src),
            RegImm { reg, val } => write!(f, "RegImm {{ reg: {:?}, val: {:?} }}", reg, val),
            Reg { src } => write!(f, "Reg {{ src: {:?} }}", src),
            Imm { val } => write!(f, "Imm {{ val: {:?} }}", val),
            Call { reg, name, args } => write!(f, "Call {{ {} @ {}({:?}) }}", reg, name, args),
            Nop => write!(f, "Nop {{ }}"),
        }
    }
}

impl fmt::Debug for IR {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use IR::*;

        match self {
            Imm(ty) => write!(f, "Imm {{ {:?} }}", ty),
            Mov(ty) => write!(f, "Mov {{ {:?} }}", ty),
            Return(ty) => write!(f, "Return {{ {:?} }}", ty),
            Load(ty) => write!(f, "Load {{ {:?} }}", ty),
            Store(ty) => write!(f, "Store {{ {:?} }}", ty),
            Unless(ty) => write!(f, "Unless {{ {:?} }}", ty),
            Label(ty) => write!(f, "Label {{ {:?} }}", ty),
            Jmp(ty) => write!(f, "Jmp {{ {:?} }}", ty),
            Add(ty) => write!(f, "Add {{ {:?} }}", ty),
            Sub(ty) => write!(f, "Sub {{ {:?} }}", ty),
            Mul(ty) => write!(f, "Mul {{ {:?} }}", ty),
            Div(ty) => write!(f, "Div {{ {:?} }}", ty),
            LessThan(ty) => write!(f, "Cmp {{ {:?} }}", ty),
            Kill(ty) => write!(f, "Kill {{ {:?} }}", ty),
            Nop(ty) => write!(f, "Nop {{ {:?} }}", ty),
            Call(ty) => write!(f, "Call {{ {:?} }}", ty),
            SaveArgs(ty) => write!(f, "Args {{ {:?} }}", ty),
        }
    }
}
