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
    Add,  // to add offsets
    Kill, // deallocate register
    Nop,
    Other(NodeType), // do we need any context here?
}

impl From<NodeType> for IRType {
    fn from(ty: NodeType) -> Self {
        match ty {
            NodeType::Constant(_) => IRType::Imm,
            t => IRType::Other(t),
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

    map: HashMap<String, i32>, // pointer offset.
    basereg: i32,
    offset: i32, // incr by 8 bytes each time
}

impl Generate {
    pub fn gen_ir(node: &Node) -> Vec<IR> {
        debug_assert!(node.ty == NodeType::Compound);

        let mut this = Self {
            inst: Vec::with_capacity(MAX_INST),

            map: HashMap::new(),
            basereg: 0,
            offset: 0,
        };

        this.inst.push(IR::new(IRType::Alloca, this.basereg, -1));
        this.gen_stmt(&node);
        // adjust the final offset
        this.inst[0].rhs = this.offset;
        this.inst.push(IR::new(IRType::Kill, this.basereg, -1));
        this.inst
    }

    fn gen_stmt(&mut self, node: &Node) {
        match node.ty {
            NodeType::Return => {
                let r = self.gen_expr(node.expr.as_ref().unwrap());
                self.inst.push(IR::new(IRType::Return, r, -1));
                self.inst.push(IR::new(IRType::Kill, r, -1));
            }
            NodeType::Expression(_) => {
                let r = self.gen_expr(node.expr.as_ref().unwrap());
                self.inst.push(IR::new(IRType::Kill, r, -1));
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
                self.inst.push(IR::new(IRType::Imm, r, *n as i32));
                return r;
            }
            NodeType::Ident(_ident) => {
                let r = self.gen_lval(node);
                self.inst.push(IR::new(IRType::Load, r, r));
                return r;
            }
            NodeType::Assign => {
                let rhs = self.gen_expr(node.rhs.as_ref().unwrap());
                let lhs = self.gen_lval(node.lhs.as_ref().unwrap());
                self.inst.push(IR::new(IRType::Store, lhs, rhs));
                self.inst.push(IR::new(IRType::Kill, rhs, -1));
                return lhs;
            }
            _ => {}
        }

        let lhs = self.gen_expr(node.lhs.as_ref().unwrap());
        let rhs = self.gen_expr(node.rhs.as_ref().unwrap());

        self.inst.push(IR::new(node.ty.clone().into(), lhs, rhs));
        self.inst.push(IR::new(IRType::Kill, rhs, -1));

        lhs
    }

    fn gen_lval(&mut self, node: &Node) -> i32 {
        if let NodeType::Ident(ident) = &node.ty {
            if !self.map.contains_key(ident) {
                self.map.insert(ident.clone(), self.offset);
                self.offset += 8;
            }

            let r1 = self.inst.len() as i32;
            self.inst.push(IR::new(IRType::Mov, r1, self.basereg));

            let offset = self.map.get(ident).expect("var to exist");
            let r2 = self.inst.len() as i32;
            self.inst.push(IR::new(IRType::Imm, r2, *offset));

            self.inst.push(IR::new(IRType::Add, r1, r2));
            self.inst.push(IR::new(IRType::Kill, r2, -1));

            return r1;
        }

        fail!("not an lvalue: {:?}", node);
    }
}

pub fn generate_x86(inst: Vec<IR>) {
    let mut label = 0;

    let mut gen_label = || {
        let s = format!(".L{}", label);
        label += 1;
        s
    };

    let ret = gen_label();

    println!("  push rbp");
    println!("  mov rbp, rsp");

    for ir in inst {
        match &ir.ty {
            IRType::Imm => {
                println!("  mov {}, {}", REGS[ir.lhs as usize], ir.rhs);
            }
            IRType::Mov => {
                println!("  mov {}, {}", REGS[ir.lhs as usize], REGS[ir.rhs as usize]);
            }
            IRType::Return => {
                println!("  mov rax, {}", REGS[ir.lhs as usize]);
                println!("  jmp {}", ret);
            }
            IRType::Alloca => {
                if ir.rhs != 0 {
                    println!("  sub rsp, {}", ir.rhs);
                }
                println!("  mov {}, rsp", REGS[ir.lhs as usize]);
            }
            IRType::Load => {
                println!(
                    "  mov {}, [{}]",
                    REGS[ir.lhs as usize], REGS[ir.rhs as usize]
                );
            }
            IRType::Store => {
                println!(
                    "  mov [{}], {}",
                    REGS[ir.lhs as usize], REGS[ir.rhs as usize]
                );
            }
            IRType::Add => {
                println!("  add {}, {}", REGS[ir.lhs as usize], REGS[ir.rhs as usize]);
            }
            // this shouldn't be like this
            IRType::Other(NodeType::Expression(e)) => match e {
                Token::Add => {
                    println!("  add {}, {}", REGS[ir.lhs as usize], REGS[ir.rhs as usize]);
                }
                Token::Sub => {
                    println!("  sub {}, {}", REGS[ir.lhs as usize], REGS[ir.rhs as usize]);
                }
                Token::Mul => {
                    println!("  mov rax, {}", REGS[ir.rhs as usize]);
                    println!("  mul {}", REGS[ir.lhs as usize]);
                    println!("  mov {}, rax", REGS[ir.lhs as usize]);
                }
                Token::Div => {
                    println!("  mov rax, {}", REGS[ir.lhs as usize]);
                    println!("  cqo");
                    println!("  div {}", REGS[ir.rhs as usize]);
                    println!("  mov {}, rax", REGS[ir.lhs as usize]);
                }
                _ => fail!("unknown irtype: {:?}", ir),
            },

            IRType::Nop => {}
            ty => fail!("unknown operator: {:?}", ty),
        }
    }

    println!("{}:", ret);
    println!("  mov rsp, rbp");
    println!("  mov rsp, rbp");
    println!("  pop rbp");
    println!("  ret");
}
