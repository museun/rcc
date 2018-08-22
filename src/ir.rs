use super::*;
use std::{
    collections::HashMap,
    fmt,
    ops::{Deref, DerefMut},
};

#[derive(Debug)]
pub struct Generate {
    inst: Vec<IR>,
    map: HashMap<String, i32>, // pointer offset.

    label: i32,
    basereg: i32,
    offset: i32, // incr by 8 bytes each time
}

impl Generate {
    pub fn gen_ir(node: &Node) -> Vec<IR> {
        match node {
            Node::Compound { .. } => {}
            _ => panic!("invalid node type: {:?}", node),
        }

        let mut this = Self {
            inst: Vec::with_capacity(MAX_INST),
            map: HashMap::new(),

            label: 0,
            basereg: 0,
            offset: 0,
        };

        this.add(IR::Alloca(reg_imm(this.basereg, -1)));

        this.gen_stmt(&node);
        // adjust the final offset
        match this.inst.get_mut(0) {
            Some(IR::Alloca(IRType::RegImm { val, .. })) => *val = this.offset,
            inst => fail!("expected alloca to be the first register. got: {:?}", inst),
        }

        this.add(IR::Kill(reg(this.basereg)));

        this.inst
    }

    fn gen_stmt(&mut self, node: &Node) {
        use IR::*;

        match node {
            Node::If { cond, body, else_ } => {
                let r = self.gen_expr(cond.as_ref().unwrap());

                let x = self.label;
                self.label += 1;

                self.add(Unless(reg_imm(r, x)));

                self.add(Kill(reg(r)));
                self.gen_stmt(body.as_ref().unwrap());

                if else_.is_none() {
                    self.add(Label(imm(x)));
                    return;
                }

                let y = self.label;
                self.label += 1;

                self.add(Jmp(imm(y)));
                self.add(Label(imm(x)));

                self.gen_stmt(else_.as_ref().unwrap());
                self.add(Label(imm(y)));
            }
            Node::Return { expr } => {
                let r = self.gen_expr(expr.as_ref().unwrap());
                self.add(Return(reg(r)));
                self.add(Kill(reg(r)));
            }
            Node::Expression { lhs, .. } => {
                let r = self.gen_expr(lhs.as_ref().unwrap());
                self.add(Kill(reg(r)));
            }
            Node::Compound { stmts } => {
                for stmt in stmts {
                    self.gen_stmt(&stmt)
                }
            }
            _ => fail!("unknown node: {:?}", node),
        }
    }

    fn gen_expr(&mut self, node: &Node) -> i32 {
        use IR::*;

        match &node {
            Node::Constant { val } => {
                let r = self.inst.len() as i32;
                self.add(AddImm(reg_imm(r, *val as i32)));
                r
            }
            Node::Ident { .. } => {
                let r = self.gen_lval(node);
                self.add(Load(reg_reg(r, r)));
                r
            }
            Node::Assign { lhs, rhs } => {
                let rhs = self.gen_expr(rhs.as_ref().unwrap());
                let lhs = self.gen_lval(lhs.as_ref().unwrap());
                self.add(Store(reg_reg(lhs, rhs)));
                self.add(Kill(reg(rhs)));
                lhs
            }
            Node::Expression { lhs, rhs, tok } => {
                let lhs = self.gen_expr(lhs.as_ref().unwrap());
                let rhs = self.gen_expr(rhs.as_ref().unwrap());
                let ty = reg_reg(lhs, rhs);
                let ir = match tok {
                    Token::Add => Add(ty),
                    Token::Sub => Sub(ty),
                    Token::Mul => Mul(ty),
                    Token::Div => Div(ty),
                    _ => fail!("invalid node type"),
                };
                self.add(ir);
                self.add(Kill(reg(rhs)));
                lhs
            }
            _ => fail!("unknown node: {:?}", node),
        }
    }

    fn gen_lval(&mut self, node: &Node) -> i32 {
        use IR::*;

        if let Node::Ident { name } = &node {
            if !self.map.contains_key(name) {
                self.map.insert(name.clone(), self.offset);
                self.offset += 8;
            }

            let r1 = self.inst.len() as i32;
            self.add(Mov(reg_reg(r1, self.basereg)));

            let offset = self.map.get(name).expect("var to exist");
            let _r2 = self.inst.len() as i32;

            self.add(Add(reg_imm(r1, *offset)));
            return r1;
        }

        fail!("not an lvalue: {:?}", node);
    }

    fn add(&mut self, ir: IR) {
        self.inst.push(ir)
    }
}

#[derive(Clone)]
pub enum IRType {
    RegReg { dst: i32, src: i32 },
    RegImm { reg: i32, val: i32 },
    Reg { src: i32 },
    Imm { val: i32 },
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
    Imm(IRType),    // reg->imm
    Mov(IRType),    // reg->reg
    Return(IRType), // reg
    Alloca(IRType), // reg->imm
    Load(IRType),   // reg->reg
    Store(IRType),  // reg->reg
    Unless(IRType), // reg->imm
    Label(IRType),  // imm
    Jmp(IRType),    // imm
    AddImm(IRType), // reg->imm
    Add(IRType),    // reg->reg
    Sub(IRType),    // reg->reg
    Mul(IRType),    // reg->reg
    Div(IRType),    // reg->reg
    Kill(IRType),   // reg
    Nop(IRType),    // nothing
}

impl Deref for IR {
    type Target = IRType;

    fn deref(&self) -> &Self::Target {
        use IR::*;
        match self {
            Imm(ty) | Mov(ty) | Return(ty) | Alloca(ty) | Load(ty) | Store(ty) | Unless(ty)
            | Label(ty) | Jmp(ty) | AddImm(ty) | Add(ty) | Sub(ty) | Mul(ty) | Div(ty)
            | Kill(ty) | Nop(ty) => ty,
        }
    }
}

impl DerefMut for IR {
    fn deref_mut(&mut self) -> &mut Self::Target {
        use IR::*;
        match self {
            Imm(ty) | Mov(ty) | Return(ty) | Alloca(ty) | Load(ty) | Store(ty) | Unless(ty)
            | Label(ty) | Jmp(ty) | AddImm(ty) | Add(ty) | Sub(ty) | Mul(ty) | Div(ty)
            | Kill(ty) | Nop(ty) => ty,
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
            Alloca(ty) => write!(f, "Alloca {{ {:?} }}", ty),
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
        }
    }
}
