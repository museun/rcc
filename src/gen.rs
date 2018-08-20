use std::cell::RefCell;

use node::{Node, NodeType};
use token::Token;

const REGISTERS: [&str; 8] = ["rdi", "rsi", "r10", "r11", "r12", "r13", "r14", "r15"];

#[derive(Debug, PartialEq, Clone)]
pub enum IRType {
    Imm,
    Mov,
    Return,
    Kill, // deallocate register
    Nop,
    Other(NodeType),
}

impl From<NodeType> for IRType {
    fn from(ty: NodeType) -> Self {
        match ty {
            NodeType::Num(_) => IRType::Imm,
            t => IRType::Other(t),
        }
    }
}

#[derive(Debug, Clone)] // whatever
pub struct IR {
    ty: IRType,
    lhs: u32,
    rhs: u32,
}

impl IR {
    pub fn new(ty: IRType, lhs: u32, rhs: u32) -> Self {
        Self { ty, lhs, rhs }
    }

    pub fn ty(&self) -> &IRType {
        &self.ty
    }

    pub fn lhs(&self) -> u32 {
        self.lhs
    }

    pub fn rhs(&self) -> u32 {
        self.rhs
    }
}

#[derive(Debug)]
pub struct Generate {
    used: [bool; 8],
    reg: u32,

    map: Vec<i32>, // probably should be an array
}

impl Generate {
    pub fn new() -> Self {
        Self {
            map: vec![-1; 1000],
            used: [false; 8],
            reg: 0,
        }
    }

    fn gen_ir_sub(&mut self, node: &Node, vec: &mut Vec<IR>) -> u32 {
        if let NodeType::Num(n) = node.ty {
            self.reg += 1;
            vec.push(IR::new(IRType::Imm, self.reg, n));
            return self.reg;
        }

        let lhs = self.gen_ir_sub(node.lhs.as_ref().unwrap(), vec);
        let rhs = self.gen_ir_sub(node.rhs.as_ref().unwrap(), vec);

        vec.push(IR::new(node.ty.clone().into(), lhs, rhs));
        vec.push(IR::new(IRType::Kill, rhs, 0));
        lhs
    }

    pub fn gen_ir(&mut self, node: &Node) -> Vec<IR> {
        let mut vec = Vec::with_capacity(1000);
        let r = self.gen_ir_sub(&node, &mut vec);
        vec.push(IR::new(IRType::Return, r, 0));
        vec
    }

    fn allocate(&mut self, r: u32) -> i32 {
        if self.map[r as usize] != -1 {
            return self.map[r as usize];
        }

        for (i, _) in REGISTERS.iter().enumerate() {
            if self.used[i] {
                continue;
            }

            self.used[i] = true;
            self.map[r as usize] = i as i32;
            return i as i32;
        }

        fail!("registers exhausted")
    }

    fn kill(&mut self, r: u32) {
        self.used[r as usize] = false;
    }

    pub fn allocate_registers(&mut self, vec: &mut Vec<IR>) -> Vec<IR> {
        let mut out = Vec::with_capacity(1000);

        for (i, ir) in vec.iter().enumerate() {
            match ir.ty {
                IRType::Imm => {
                    let lhs = self.allocate(ir.lhs()) as u32;
                    out[i as usize] = {
                        let mut ir = ir.clone();
                        ir.lhs = lhs;
                        ir
                    };
                }
                IRType::Mov | IRType::Other(_) => {
                    let lhs = self.allocate(ir.lhs()) as u32;
                    let rhs = self.allocate(ir.rhs()) as u32;
                    out[i as usize] = {
                        let mut ir = ir.clone();
                        ir.lhs = lhs;
                        ir.rhs = rhs;
                        ir
                    };
                }
                IRType::Return => {
                    let lhs = self.map[ir.lhs as usize];
                    self.kill(lhs as u32);
                }
                IRType::Kill => {
                    let lhs = self.map[ir.lhs as usize];
                    self.kill(lhs as u32);
                    let lhs = self.allocate(ir.lhs()) as u32;
                    out[i as usize] = {
                        let mut ir = ir.clone();
                        ir.ty = IRType::Nop;
                        ir
                    };
                }
                IRType::Nop => {
                    // do nothing
                }
            }
        }

        out
    }

    pub fn generate(&self, vec: &mut Vec<IR>) {
        for ir in vec.iter() {
            match &ir.ty {
                IRType::Imm => {
                    println!("  mov {}, {}", self.map[ir.lhs as usize], ir.rhs);
                }
                IRType::Mov => {
                    println!(
                        "   mov {}, {}",
                        self.map[ir.lhs as usize], self.map[ir.rhs as usize]
                    );
                }
                IRType::Return => {
                    println!("  mov rax {}", self.map[ir.lhs as usize]);
                    println!("  ret\n");
                }

                IRType::Other(NodeType::Add) => {
                    println!(
                        "  add {}, {}",
                        self.map[ir.lhs as usize], self.map[ir.rhs as usize]
                    );
                }
                IRType::Other(NodeType::Sub) => {
                    println!(
                        "  sub {}, {}",
                        self.map[ir.lhs as usize], self.map[ir.rhs as usize]
                    );
                }
                IRType::Nop => {
                    // do nothing
                }
                ty => {
                    fail!("unknown operator: {:?}", ty);
                }
            }
        }
    }
}
