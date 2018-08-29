use ir::{Function, IRType, IR};

use std::{mem, ops::DerefMut};

pub struct Registers {
    used: [bool; 7],
    map: Vec<i32>,
}

impl Registers {
    pub fn allocate(funcs: &mut [Function]) -> &mut [Function] {
        for func in funcs.iter_mut() {
            let len = ::std::cmp::max(func.ir.len(), 1);
            let mut this = Self {
                used: [false; 7],
                map: vec![-1; len],
            };
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
        use ir::IRType::*;

        for ir in inst.iter_mut() {
            if let IR::Kill(Reg { src }) = &ir {
                let r = self.map[*src as usize] as usize;

                debug_assert!(self.used[r]);
                self.used[r] = false;

                *ir = IR::Nop(IRType::Nop);
                continue;
            };

            match ir.deref_mut() {
                Cmp { dst, src, .. } | RegReg { dst, src } => {
                    *dst = self.alloc(*dst);
                    *src = self.alloc(*src);
                }

                RegLabel { reg, .. } | RegImm { reg, .. } | Reg { src: reg } => {
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

        for i in 0..self.used.len() {
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
