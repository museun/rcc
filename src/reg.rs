use super::*;
use std::ops::DerefMut;

pub(crate) const REGS: [&str; 8] = ["rbp", "r10", "r11", "rbx", "r12", "r13", "r14", "r15"];
pub(crate) const REGS8: [&str; 8] = ["bpl", "r10b", "r11b", "bl", "r12b", "r13b", "r14b", "r15b"];

pub struct Registers {
    used: [bool; 8],
    map: Vec<i32>,
}

impl Registers {
    pub fn allocate(funcs: &mut Vec<Function>) {
        for func in funcs {
            let mut this = Self {
                used: [false; 8],
                map: vec![-1; func.ir.len()],
            };
            // first register will be reserved for rbp
            this.map[0] = 0;
            this.used[0] = true;

            this.visit(&mut func.ir);
        }
    }

    fn visit(&mut self, inst: &mut Vec<IR>) {
        use IRType::*;

        for ir in inst.iter_mut() {
            if let IR::Kill(Reg { src }) = &ir {
                self.kill(self.map[*src as usize]);
                *ir = IR::Nop(IRType::Nop);
                continue;
            };

            match ir.deref_mut() {
                RegReg { dst, src } => {
                    *dst = self.alloc(*dst);
                    *src = self.alloc(*src);
                }
                Reg { src } => *src = self.alloc(*src),
                RegImm { reg, .. } => {
                    *reg = self.alloc(*reg);
                }
                IRType::Call { reg, args, .. } => {
                    *reg = self.alloc(*reg);
                    for arg in args {
                        *arg = self.alloc(*arg)
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

            self.map[r as usize] = i as i32;
            self.used[i] = true;
            return i as i32;
        }

        fail!("registers exhausted")
    }

    fn kill(&mut self, r: i32) {
        debug_assert!(self.used[r as usize]);
        self.used[r as usize] = false;
    }
}
