use super::*;
use node::Node;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum IRType {
    Imm,
    Mov,
    Return,
    Alloca,
    Load,
    Store,

    Unless,
    Label,
    Jmp,

    // from tokens
    Add(Option<i32>),
    Sub,
    Mul,
    Div,

    Kill, // deallocate register
    Nop,
}

impl From<Token> for IRType {
    fn from(tok: Token) -> Self {
        match tok {
            Token::Add => IRType::Add(None),
            Token::Sub => IRType::Sub,
            Token::Mul => IRType::Mul,
            Token::Div => IRType::Div,
            _ => fail!("invalid node type"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct IR {
    pub(crate) ty: IRType,
    pub(crate) lhs: i32,
    pub(crate) rhs: i32,
}

impl IR {
    fn new(ty: IRType, lhs: i32, rhs: i32) -> Self {
        Self { ty, lhs, rhs }
    }
}

#[derive(Debug)]
pub struct Generate {
    inst: Vec<IR>,
    label: i32,
    map: HashMap<String, i32>, // pointer offset.
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
            label: 0,
            map: HashMap::new(),
            basereg: 0,
            offset: 0,
        };

        this.add(IRType::Alloca, this.basereg, -1);
        this.gen_stmt(&node);
        // adjust the final offset
        this.inst[0].rhs = this.offset;
        this.add(IRType::Kill, this.basereg, -1);
        this.inst
    }

    fn gen_stmt(&mut self, node: &Node) {
        use IRType::*;

        match node {
            Node::If { cond, body, else_ } => {
                let r = self.gen_expr(cond.as_ref().unwrap());

                let x = self.label;
                self.label += 1;

                self.add(Unless, r, x);
                // TODO figure out why killing this register breaks it
                // self.add(Kill, r, -1);
                self.gen_stmt(body.as_ref().unwrap());

                if else_.is_none() {
                    self.add(Label, x, -1);
                    return;
                }

                let y = self.label;
                self.label += 1;

                self.add(Jmp, y, -1);
                self.add(Label, x, -1);
                self.gen_stmt(else_.as_ref().unwrap());
                self.add(Label, y, -1);
            }
            Node::Return { expr } => {
                let r = self.gen_expr(expr.as_ref().unwrap());
                self.add(Return, r, -1);
                self.add(Kill, r, -1);
            }
            Node::Expression { lhs, .. } => {
                let r = self.gen_expr(lhs.as_ref().unwrap());
                self.add(Kill, r, -1);
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
        match &node {
            Node::Constant { val } => {
                let r = self.inst.len() as i32;
                self.add(IRType::Imm, r, *val as i32);
                r
            }
            Node::Ident { .. } => {
                let r = self.gen_lval(node);
                self.add(IRType::Load, r, r);
                r
            }
            Node::Assign { lhs, rhs } => {
                let rhs = self.gen_expr(rhs.as_ref().unwrap());
                let lhs = self.gen_lval(lhs.as_ref().unwrap());
                self.add(IRType::Store, lhs, rhs);
                self.add(IRType::Kill, rhs, -1);
                lhs
            }
            Node::Expression { lhs, rhs, tok } => {
                let lhs = self.gen_expr(lhs.as_ref().unwrap());
                let rhs = self.gen_expr(rhs.as_ref().unwrap());
                self.add(tok.clone().into(), lhs, rhs);
                self.add(IRType::Kill, rhs, -1);
                lhs
            }
            _ => unreachable!(),
        }
    }

    fn gen_lval(&mut self, node: &Node) -> i32 {
        if let Node::Ident { name } = &node {
            if !self.map.contains_key(name) {
                self.map.insert(name.clone(), self.offset);
                self.offset += 8;
            }

            let r1 = self.inst.len() as i32;
            self.add(IRType::Mov, r1, self.basereg);

            let offset = self.map.get(name).expect("var to exist");
            let r2 = self.inst.len() as i32;
            self.add(IRType::Add(Some(*offset)), r1, r2);
            return r1;
        }

        fail!("not an lvalue: {:?}", node);
    }

    fn add(&mut self, ty: IRType, lhs: i32, rhs: i32) {
        self.inst.push(IR::new(ty, lhs, rhs))
    }
}
