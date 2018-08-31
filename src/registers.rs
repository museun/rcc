use ir::{Function, IRKind, IR};
use std::mem;

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
        let nop = mem::discriminant(&IRKind::Nop);
        funcs
            .iter_mut()
            .for_each(|f| f.ir.retain(|ir| mem::discriminant(&ir.kind) != nop));

        funcs
    }

    fn visit(&mut self, inst: &mut Vec<IR>) {
        use ir::IRType::*;

        for ir in inst.iter_mut() {
            if let IRKind::Kill = &ir.kind {
                let src = match ir.ty {
                    Imm { val } => val,
                    _ => unreachable!(),
                };

                let r = self.map[src as usize] as usize;
                debug_assert!(self.used[r]);
                self.used[r] = false;

                ir.kind = IRKind::Nop;
                continue;
            };

            match &mut ir.ty {
                Cmp { dst, src, .. } | RegReg { dst, src } => {
                    *dst = self.alloc(*dst);
                    *src = self.alloc(*src);
                }

                RegLabel { reg, .. } | RegImm { reg, .. } | Reg { src: reg } => {
                    *reg = self.alloc(*reg);
                }

                Call { reg, args, .. } => {
                    *reg = self.alloc(*reg);
                    for arg in args {
                        *arg = self.alloc(*arg)
                    }
                }

                Imm { .. } | Nop => {
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
