use super::*;

// this should be a trait
pub fn generate_x86(inst: Vec<IR>) {
    let ret = ".Lend";

    println!("  push rbp");
    println!("  mov rbp, rsp");

    use IRType::*;
    for ir in inst {
        match &ir.ty {
            Imm => {
                println!("  mov {}, {}", REGS[ir.lhs as usize], ir.rhs);
            }
            Mov => {
                println!("  mov {}, {}", REGS[ir.lhs as usize], REGS[ir.rhs as usize]);
            }
            Return => {
                println!("  mov rax, {}", REGS[ir.lhs as usize]);
                println!("  jmp {}", ret);
            }
            Alloca => {
                if ir.rhs != 0 {
                    println!("  sub rsp, {}", ir.rhs);
                }
                println!("  mov {}, rsp", REGS[ir.lhs as usize]);
            }
            Load => {
                println!(
                    "  mov {}, [{}]",
                    REGS[ir.lhs as usize], REGS[ir.rhs as usize]
                );
            }
            Store => {
                println!(
                    "  mov [{}], {}",
                    REGS[ir.lhs as usize], REGS[ir.rhs as usize]
                );
            }
            Label => {
                println!(".L{}:", ir.lhs);
            }
            Unless => {
                println!("  cmp {}, 0", REGS[ir.lhs as usize]);
                println!("  je .L{}", ir.rhs)
            }
            Jmp => {
                println!("  jmp .L{}", ir.lhs);
            }
            Add(None) => {
                println!("  add {}, {}", REGS[ir.lhs as usize], REGS[ir.rhs as usize]);
            }
            Add(Some(v)) => {
                println!("  add {}, {}", REGS[ir.lhs as usize], v);
            }
            Sub => {
                println!("  sub {}, {}", REGS[ir.lhs as usize], REGS[ir.rhs as usize]);
            }
            Mul => {
                println!("  mov rax, {}", REGS[ir.rhs as usize]);
                println!("  mul {}", REGS[ir.lhs as usize]);
                println!("  mov {}, rax", REGS[ir.lhs as usize]);
            }
            Div => {
                println!("  mov rax, {}", REGS[ir.lhs as usize]);
                println!("  cqo");
                println!("  div {}", REGS[ir.rhs as usize]);
                println!("  mov {}, rax", REGS[ir.lhs as usize]);
            }

            Nop => {}
            ty => fail!("unknown operator: {:?}", ty),
        }
    }

    println!("{}:", ret);
    println!("  mov rsp, rbp");
    println!("  mov rsp, rbp");
    println!("  pop rbp");
    println!("  ret");
}
