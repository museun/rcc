use super::*;
use node::{Node, NodeType};
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

    // from tokens
    Add(Option<i32>),
    Sub,
    Mul,
    Div,

    Kill, // deallocate register
    Nop,
}

// this is awful
impl From<NodeType> for IRType {
    fn from(ty: NodeType) -> Self {
        match ty {
            NodeType::Constant(_) => IRType::Imm,
            NodeType::Expression(t) => match t {
                Token::Add => IRType::Add(None),
                Token::Sub => IRType::Sub,
                Token::Mul => IRType::Mul,
                Token::Div => IRType::Div,
                _ => fail!("invalid node type"),
            },
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
    pub fn new(ty: IRType, lhs: i32, rhs: i32) -> Self {
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
        debug_assert!(node.ty == NodeType::Compound);

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
        match node.ty {
            NodeType::If => {
                let r = self.gen_expr(node.cond.as_ref().unwrap());
                let j = self.label;
                self.label += 1;
                self.add(IRType::Unless, r, j);
                self.add(IRType::Kill, r, -1);
                self.gen_stmt(node.then.as_ref().unwrap());
                self.add(IRType::Label, j, -1);
            }
            NodeType::Return => {
                let r = self.gen_expr(node.expr.as_ref().unwrap());
                self.add(IRType::Return, r, -1);
                self.add(IRType::Kill, r, -1);
            }
            NodeType::Expression(_) => {
                let r = self.gen_expr(node.expr.as_ref().unwrap());
                self.add(IRType::Kill, r, -1);
            }
            NodeType::Compound => {
                for stmt in &node.stmts {
                    self.gen_stmt(&stmt)
                }
            }
            _ => fail!("unknown node: {:?}", node),
        }
    }

    fn gen_expr(&mut self, node: &Node) -> i32 {
        match &node.ty {
            NodeType::Constant(n) => {
                let r = self.inst.len() as i32;
                self.add(IRType::Imm, r, *n as i32);
                return r;
            }
            NodeType::Ident(_ident) => {
                let r = self.gen_lval(node);
                self.add(IRType::Load, r, r);
                return r;
            }
            NodeType::Assign => {
                let rhs = self.gen_expr(node.rhs.as_ref().unwrap());
                let lhs = self.gen_lval(node.lhs.as_ref().unwrap());
                self.add(IRType::Store, lhs, rhs);
                self.add(IRType::Kill, rhs, -1);
                return lhs;
            }
            _ => {}
        }

        let lhs = self.gen_expr(node.lhs.as_ref().unwrap());
        let rhs = self.gen_expr(node.rhs.as_ref().unwrap());

        // TODO `fix` this line
        self.add(node.ty.clone().into(), lhs, rhs);
        self.add(IRType::Kill, rhs, -1);

        lhs
    }

    fn gen_lval(&mut self, node: &Node) -> i32 {
        if let NodeType::Ident(ident) = &node.ty {
            if !self.map.contains_key(ident) {
                self.map.insert(ident.clone(), self.offset);
                self.offset += 8;
            }

            let r1 = self.inst.len() as i32;
            self.add(IRType::Mov, r1, self.basereg);

            let offset = self.map.get(ident).expect("var to exist");
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

pub fn generate_x86(inst: Vec<IR>) {
    let ret = ".Lend";

    println!("  push rbp");
    println!("  mov rbp, rsp");

    use IRType::*;
    for ir in inst {
        match &ir.ty {
            Imm => {
                println!("  mov {}, {}", REGS[ir.lhs as usize], ir.rhs);
            }
            Mov => {
                println!("  mov {}, {}", REGS[ir.lhs as usize], REGS[ir.rhs as usize]);
            }
            Return => {
                println!("  mov rax, {}", REGS[ir.lhs as usize]);
                println!("  jmp {}", ret);
            }
            Alloca => {
                if ir.rhs != 0 {
                    println!("  sub rsp, {}", ir.rhs);
                }
                println!("  mov {}, rsp", REGS[ir.lhs as usize]);
            }
            Load => {
                println!(
                    "  mov {}, [{}]",
                    REGS[ir.lhs as usize], REGS[ir.rhs as usize]
                );
            }
            Store => {
                println!(
                    "  mov [{}], {}",
                    REGS[ir.lhs as usize], REGS[ir.rhs as usize]
                );
            }
            Label => {
                println!(".L{}:", ir.lhs);
            }
            Unless => {
                println!("  cmp {}, 0", REGS[ir.lhs as usize]);
                println!("  je .L{}", ir.rhs)
            }
            Add(None) => {
                println!("  add {}, {}", REGS[ir.lhs as usize], REGS[ir.rhs as usize]);
            }
            Add(Some(v)) => {
                println!("  add {}, {}", REGS[ir.lhs as usize], v);
            }
            Sub => {
                println!("  sub {}, {}", REGS[ir.lhs as usize], REGS[ir.rhs as usize]);
            }
            Mul => {
                println!("  mov rax, {}", REGS[ir.rhs as usize]);
                println!("  mul {}", REGS[ir.lhs as usize]);
                println!("  mov {}, rax", REGS[ir.lhs as usize]);
            }
            Div => {
                println!("  mov rax, {}", REGS[ir.lhs as usize]);
                println!("  cqo");
                println!("  div {}", REGS[ir.rhs as usize]);
                println!("  mov {}, rax", REGS[ir.lhs as usize]);
            }

            Nop => {}
            ty => fail!("unknown operator: {:?}", ty),
        }
    }

    println!("{}:", ret);
    println!("  mov rsp, rbp");
    println!("  mov rsp, rbp");
    println!("  pop rbp");
    println!("  ret");
}
