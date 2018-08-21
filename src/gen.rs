use node::Node;
use token::Token;

//use token::Token;

const REGS: [&str; 8] = ["rdi", "rsi", "r10", "r11", "r12", "r13", "r14", "r15"];

#[derive(Debug, PartialEq, Clone)]
pub enum IRType {
    Imm,
    Mov,
    Return,
    Kill, // deallocate register
    Nop,
    Other(Token),
}

impl From<Token> for IRType {
    fn from(ty: Token) -> Self {
        match ty {
            Token::Num(_) => IRType::Imm,
            t => IRType::Other(t),
        }
    }
}

#[derive(Debug, Clone)] // whatever
pub struct IR {
    ty: IRType,
    lhs: i32,
    rhs: i32,
}

impl IR {
    pub fn new(ty: IRType, lhs: i32, rhs: i32) -> Self {
        Self { ty, lhs, rhs }
    }
}

#[derive(Debug)]
pub struct Generate {
    reg: i32,
    inst: Vec<IR>,
    used: [bool; 8],
    map: Vec<i32>,
}

impl Generate {
    pub fn new() -> Self {
        Self {
            reg: 0,
            inst: Vec::with_capacity(1000),
            used: [false; 8],
            map: vec![-1; 1000],
        }
    }

    fn gen_ir_sub(&mut self, node: &Node) -> i32 {
        if let Token::Num(n) = node.ty {
            let r = self.inst.len() as i32;
            self.inst.push(IR::new(IRType::Imm, r, n as i32));
            return r;
        }

        let lhs = self.gen_ir_sub(node.lhs.as_ref().unwrap());
        let rhs = self.gen_ir_sub(node.rhs.as_ref().unwrap());

        self.inst.push(IR::new(node.ty.clone().into(), lhs, rhs));
        self.inst.push(IR::new(IRType::Kill, rhs, 0));
        lhs
    }

    pub fn gen_ir(&mut self, node: &Node) {
        let r = self.gen_ir_sub(&node);
        self.inst.push(IR::new(IRType::Return, r, 0));
    }

    fn allocate(&mut self, r: i32) -> i32 {
        if self.map[r as usize] != -1 {
            let r = self.map[r as usize];
            debug_assert!(self.used[r as usize]);
            return r;
        }

        for i in 0..REGS.len() {
            if self.used[i] {
                continue;
            }

            self.used[i] = true;
            self.map[r as usize] = i as i32;
            return i as i32;
        }

        fail!("registers exhausted")
    }

    fn kill(&mut self, r: i32) {
        debug_assert!(self.used[r as usize]);
        self.used[r as usize] = false;
    }

    pub fn allocate_registers(&mut self) {
        let mut inst = self.inst.to_vec();

        for i in 0..inst.len() {
            let ir = inst.get_mut(i).expect("get ir");

            match ir.ty {
                IRType::Imm => {
                    ir.lhs = self.allocate(ir.lhs);
                }
                IRType::Mov | IRType::Other(_) => {
                    ir.lhs = self.allocate(ir.lhs);
                    ir.rhs = self.allocate(ir.rhs);
                }
                IRType::Return => {
                    self.kill(self.map[ir.lhs as usize]);
                }
                IRType::Kill => {
                    self.kill(self.map[ir.lhs as usize]);
                    ir.ty = IRType::Nop;
                }
                IRType::Nop => {
                    // do nothing
                }
            }
        }

        ::std::mem::replace(&mut self.inst, inst);
    }

    pub fn generate(&self) {
        for ir in &self.inst {
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
                IRType::Other(Token::Add) => {
                    println!("  add {}, {}", REGS[ir.lhs as usize], REGS[ir.rhs as usize]);
                }
                IRType::Other(Token::Sub) => {
                    println!("  sub {}, {}", REGS[ir.lhs as usize], REGS[ir.rhs as usize]);
                }
                IRType::Other(Token::Mul) => {
                    println!("  mov rax, {}", REGS[ir.rhs as usize]);
                    println!("  mul {}", REGS[ir.lhs as usize]);
                    println!("  mov {}, rax", REGS[ir.lhs as usize]);
                }
                IRType::Other(Token::Div) => {
                    println!("  mov rax, {}", REGS[ir.lhs as usize]);
                    println!("  cqo");
                    println!("  div {}", REGS[ir.rhs as usize]);
                    println!("  mov {}, rax", REGS[ir.lhs as usize]);
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

impl Default for Generate {
    fn default() -> Self {
        Self::new()
    }
}
