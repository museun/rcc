use super::*;
use node::{Node, NodeType};

#[derive(Debug, PartialEq, Clone)]
pub enum IRType {
    Imm,
    Mov,
    Return,
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
}

impl Generate {
    pub fn gen_ir(node: &Node) -> Vec<IR> {
        debug_assert!(node.ty == NodeType::Compound);

        let mut this = Self {
            inst: Vec::with_capacity(MAX_INST),
        };

        this.gen_stmt(&node);
        this.inst
    }

    fn gen_expr(&mut self, node: &Node) -> i32 {
        if let NodeType::Constant(n) = node.ty {
            let r = self.inst.len() as i32;
            self.inst.push(IR::new(IRType::Imm, r, n as i32));
            return r;
        }

        let lhs = self.gen_expr(node.lhs.as_ref().unwrap());
        let rhs = self.gen_expr(node.rhs.as_ref().unwrap());

        self.inst.push(IR::new(node.ty.clone().into(), lhs, rhs));
        self.inst.push(IR::new(IRType::Kill, rhs, 0));

        lhs
    }

    fn gen_stmt(&mut self, node: &Node) {
        match node.ty {
            NodeType::Return => {
                let r = self.gen_expr(node.expr.as_ref().unwrap());
                self.inst.push(IR::new(IRType::Return, r, 0));
                self.inst.push(IR::new(IRType::Kill, r, 0));
            }
            NodeType::Expression(_) => {
                let r = self.gen_expr(node.expr.as_ref().unwrap());
                self.inst.push(IR::new(IRType::Kill, r, 0));
            }
            NodeType::Compound => {
                for stmt in &node.stmts {
                    self.gen_stmt(&stmt)
                }
            }
            _ => fail!("unknown node: {:?}", node),
        }
    }
}

pub fn generate_x86(inst: Vec<IR>) {
    for ir in inst {
        match &ir.ty {
            IRType::Imm => {
                println!("  mov {}, {}", REGS[ir.lhs as usize], ir.rhs);
            }
            IRType::Mov => {
                println!(
                    "   mov {}, {}",
                    REGS[ir.lhs as usize], REGS[ir.rhs as usize]
                );
            }
            IRType::Return => {
                println!("  mov rax, {}", REGS[ir.lhs as usize]);
                println!("  ret");
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
                _ => {
                    fail!("unknown irtype: {:?}", ir);
                }
            },

            IRType::Nop => {}
            ty => fail!("unknown operator: {:?}", ty),
        }
    }
}
