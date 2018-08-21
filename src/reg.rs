use super::*;

pub struct Registers {
    used: [bool; 8],
    map: Vec<i32>,
}

impl Registers {
    pub fn allocate(inst: &mut Vec<IR>) {
        let mut this = Self {
            used: [false; 8],
            map: vec![-1; MAX_INST],
        };

        use IRType::*;

        for ir in inst.iter_mut() {
            match &ir.ty {
                Imm | Alloca | Return => ir.lhs = this.alloc(ir.lhs),
                Kill => {
                    this.kill(this.map[ir.lhs as usize]);
                    ir.ty = Nop;
                }
                Mov | Load | Store | Sub | Mul | Div | Add(None) => {
                    ir.lhs = this.alloc(ir.lhs);
                    ir.rhs = this.alloc(ir.rhs);
                }
                Add(Some(_)) => {
                    ir.lhs = this.alloc(ir.lhs);
                }
                Nop => {
                    // do nothing
                }
            }
        }
    }

    fn alloc(&mut self, r: i32) -> i32 {
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
}