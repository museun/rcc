use super::*;
use std::{mem, ops::DerefMut};

pub(crate) const REGS: [&str; 8] = ["rbp", "r10", "r11", "rbx", "r12", "r13", "r14", "r15"];
pub(crate) const REGS8: [&str; 8] = ["bpl", "r10b", "r11b", "bl", "r12b", "r13b", "r14b", "r15b"];
pub(crate) const REGS32: [&str; 8] = ["ebp", "r10d", "r11d", "ebx", "e12d", "e13d", "r14d", "r15d"];

pub struct Registers {
    used: [bool; 8],
    map: Vec<i32>,
}

impl Registers {
    pub fn allocate(funcs: &mut [Function]) -> &mut [Function] {
        for func in funcs.iter_mut() {
            let mut this = Self {
                used: [false; 8],
                map: vec![-1; func.ir.len()],
            };
            // first register will be reserved for rbp
            this.map[0] = 0;
            this.used[0] = true;

            this.visit(&mut func.ir);
        }

        // remove all of the no-ops
        let nop = mem::discriminant(&IR::Nop(IRType::Nop {}));
        funcs
            .iter_mut()
            .for_each(|f| f.ir.retain(|ir| mem::discriminant(ir) != nop));

        funcs
    }

    fn visit(&mut self, inst: &mut Vec<IR>) {
        use IRType::*;

        for ir in inst.iter_mut() {
            if let IR::Kill(Reg { src }) = &ir {
                let r = self.map[*src as usize] as usize;

                debug_assert!(self.used[r]);
                self.used[r] = false;

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
}
