use super::*;
use std::{
    collections::HashMap,
    fmt,
    ops::{Deref, DerefMut},
};

#[derive(Debug)]
pub struct Function {
    pub(crate) name: String,
    pub(crate) args: Vec<i32>, // could be an array
    pub(crate) stacksize: i32,
    pub(crate) ir: Vec<IR>,
}

#[derive(Debug)]
pub struct Generate {
    inst: Vec<IR>,
    map: HashMap<String, i32>, // pointer offset.
    label: i32,
    stacksize: i32,
}

impl Generate {
    pub fn gen_ir(nodes: &[Node]) -> Vec<Function> {
        let mut out = vec![];
        for node in nodes {
            match node {
                Node::Func { name, body, args } => {
                    let mut this = Self {
                        // TODO be smarter about this
                        inst: Vec::with_capacity(MAX_INST),
                        map: HashMap::new(),
                        label: 0,
                        stacksize: 0,
                    };

                    this.gen_args(args);

                    this.gen_stmt(body.as_ref().unwrap());
                    let function = Function {
                        name: name.clone(),
                        args: vec![],
                        stacksize: this.stacksize,
                        ir: this.inst,
                    };
                    out.push(function);
                }
                node => fail!("expected a function node, got: {:?} ", node),
            }
        }
        out
    }

    fn gen_args(&mut self, nodes: &[Node]) {
        if nodes.is_empty() {
            return;
        }

        self.add(IR::SaveArgs(imm(nodes.len() as i32)));
        for node in nodes {
            match node {
                Node::Ident { name } => {
                    self.stacksize += 8;
                    self.map.insert(name.clone(), self.stacksize);
                }
                _ => fail!("bad parameter"),
            }
        }
    }

    fn gen_stmt(&mut self, node: &Node) {
        match node {
            Node::If { cond, body, else_ } => {
                let r = self.gen_expr(cond.as_ref().unwrap());
                let x = self.label;
                self.label += 1;

                self.add(IR::Unless(reg_imm(r, x)));
                self.add(IR::Kill(reg(r)));

                self.gen_stmt(body.as_ref().unwrap());

                if else_.is_none() {
                    self.add(IR::Label(imm(x)));
                    return;
                }

                let y = self.label;
                self.label += 1;

                self.add(IR::Jmp(imm(y)));
                self.add(IR::Label(imm(x)));

                self.gen_stmt(else_.as_ref().unwrap());
                self.add(IR::Label(imm(y)));
            }
            Node::Return { expr } => {
                let r = self.gen_expr(expr.as_ref().unwrap());
                self.add(IR::Return(reg(r)));
                self.add(IR::Kill(reg(r)));
            }
            Node::Expression { lhs, .. } => {
                let r = self.gen_expr(lhs.as_ref().unwrap());
                self.add(IR::Kill(reg(r)));
            }
            Node::Compound { stmts } => {
                for stmt in stmts {
                    self.gen_stmt(&stmt)
                }
            }
            _ => fail!("unknown node in stmt: {:?}", node),
        }
    }

    fn gen_expr(&mut self, node: &Node) -> i32 {
        match &node {
            Node::Constant { val } => {
                let r = (self.inst.len() + 1) as i32;
                self.add(IR::AddImm(reg_imm(r, *val as i32)));
                r
            }
            Node::Ident { .. } => {
                let r = self.gen_lval(node);
                self.add(IR::Load(reg_reg(r, r)));
                r
            }
            Node::Call { name, args } => {
                let mut vec = vec![];
                for arg in args {
                    vec.push(self.gen_expr(arg))
                }

                let r = (self.inst.len() + 1) as i32;
                self.add(IR::Call(IRType::Call {
                    reg: r,
                    name: name.clone(),
                    args: vec.clone(),
                }));

                for k in vec {
                    self.add(IR::Kill(reg(k)));
                }
                r
            }

            Node::Assign { lhs, rhs } => {
                let rhs = self.gen_expr(rhs.as_ref().unwrap());
                let lhs = self.gen_lval(lhs.as_ref().unwrap());
                self.add(IR::Store(reg_reg(lhs, rhs)));
                self.add(IR::Kill(reg(rhs)));
                lhs
            }
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
            _ => fail!("unknown node in expr: {:?}", node),
        }
    }

    fn gen_lval(&mut self, node: &Node) -> i32 {
        if let Node::Ident { name } = &node {
            if !self.map.contains_key(name) {
                self.stacksize += 8;
                self.map.insert(name.clone(), self.stacksize);
            }

            let r = (self.inst.len() + 1) as i32;
            self.add(IR::Mov(reg_reg(r, 0)));

            let offset = self.map.get(name).expect("var to exist");
            self.add(IR::Sub(reg_imm(r, *offset)));
            return r;
        }

        fail!("not an lvalue: {:?}", node);
    }

    fn add(&mut self, ir: IR) {
        self.inst.push(ir)
    }
}

#[derive(Clone)]
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

// helpers to make IRTypes

fn reg_reg(dst: i32, src: i32) -> IRType {
    IRType::RegReg { dst, src }
}

fn reg_imm(reg: i32, val: i32) -> IRType {
    IRType::RegImm { reg, val }
}

fn reg(src: i32) -> IRType {
    IRType::Reg { src }
}

fn imm(val: i32) -> IRType {
    IRType::Imm { val }
}

#[derive(Clone)]
pub enum IR {
    Imm(IRType),      // reg->imm
    Mov(IRType),      // reg->reg
    Return(IRType),   // reg
    Load(IRType),     // reg->reg
    Store(IRType),    // reg->reg
    Unless(IRType),   // reg->imm
    Label(IRType),    // imm
    Jmp(IRType),      // imm
    AddImm(IRType),   // reg->imm
    Add(IRType),      // reg->reg
    Sub(IRType),      // reg->reg
    Mul(IRType),      // reg->reg
    Div(IRType),      // reg->reg
    Kill(IRType),     // reg
    Nop(IRType),      // nothing
    Call(IRType),     // call name, [args]
    SaveArgs(IRType), // args + stack offset
}

impl Deref for IR {
    type Target = IRType;

    fn deref(&self) -> &Self::Target {
        use IR::*;
        match self {
            Imm(ty) | Mov(ty) | Return(ty) | Load(ty) | Store(ty) | Unless(ty) | Label(ty)
            | Jmp(ty) | AddImm(ty) | Add(ty) | Sub(ty) | Mul(ty) | Div(ty) | Kill(ty) | Nop(ty)
            | Call(ty) | SaveArgs(ty) => ty,
        }
    }
}

impl DerefMut for IR {
    fn deref_mut(&mut self) -> &mut Self::Target {
        use IR::*;
        match self {
            Imm(ty) | Mov(ty) | Return(ty) | Load(ty) | Store(ty) | Unless(ty) | Label(ty)
            | Jmp(ty) | AddImm(ty) | Add(ty) | Sub(ty) | Mul(ty) | Div(ty) | Kill(ty) | Nop(ty)
            | Call(ty) | SaveArgs(ty) => ty,
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
            AddImm(ty) => write!(f, "AddImm {{ {:?} }}", ty),
            Add(ty) => write!(f, "Add {{ {:?} }}", ty),
            Sub(ty) => write!(f, "Sub {{ {:?} }}", ty),
            Mul(ty) => write!(f, "Mul {{ {:?} }}", ty),
            Div(ty) => write!(f, "Div {{ {:?} }}", ty),
            Kill(ty) => write!(f, "Kill {{ {:?} }}", ty),
            Nop(ty) => write!(f, "Nop {{ {:?} }}", ty),
            Call(ty) => write!(f, "Call {{ {:?} }}", ty),
            SaveArgs(ty) => write!(f, "Args {{ {:?} }}", ty),
        }
    }
}
