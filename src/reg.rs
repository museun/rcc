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

        use std::ops::DerefMut;

        use IRType::*;
        use IR::*;

        for ir in inst.iter_mut() {
            if let Kill(Reg { src }) = &ir {
                this.kill(this.map[*src as usize]);
                *ir = IR::Nop(IRType::Nop);
                continue;
            };

            // shit
            match ir.deref_mut() {
                RegReg { dst, src } => {
                    *dst = this.alloc(*dst);
                    *src = this.alloc(*src);
                }
                Reg { src } => *src = this.alloc(*src),
                RegImm { reg, .. } => {
                    *reg = this.alloc(*reg);
                }
                IRType::Call { reg, args, .. } => {
                    *reg = this.alloc(*reg);
                    for arg in args {
                        *arg = this.alloc(*arg)
                    }
                }
                IRType::Imm { .. } | IRType::Nop { .. } => {
                    // doesn't need register allocations
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
