use super::*;

// this should be a trait
pub fn generate_x86(inst: Vec<IR>) {
    let ret = ".Lend";

    println!("  push rbp");
    println!("  mov rbp, rsp");

    use IRType::*;
    use IR::*;

    for ir in inst {
        match &ir {
            IR::Imm(RegImm { reg, val }) => {
                println!("  mov {}, {}", REGS[*reg as usize], val);
            }
            Mov(RegReg { dst, src }) => {
                println!("  mov {}, {}", REGS[*dst as usize], REGS[*src as usize]);
            }
            Return(Reg { src }) => {
                println!("  mov rax, {}", REGS[*src as usize]);
                println!("  jmp {}", ret);
            }
            Alloca(RegImm { reg, val }) => {
                if *val != 0 {
                    println!("  sub rsp, {}", val);
                }
                println!("  mov {}, rsp", REGS[*reg as usize]);
            }
            Load(RegReg { dst, src }) => {
                println!("  mov {}, [{}]", REGS[*dst as usize], REGS[*src as usize]);
            }
            Store(RegReg { dst, src }) => {
                println!("  mov [{}], {}", REGS[*dst as usize], REGS[*src as usize]);
            }
            Label(IRType::Imm { val }) => println!(".L{}:", val),
            Unless(RegImm { reg, val }) => {
                println!("  cmp {}, 0", REGS[*reg as usize]);
                println!("  je .L{}", val)
            }
            Jmp(IRType::Imm { val }) => println!("  jmp .L{}", val),
            Add(RegReg { dst, src }) => {
                println!("  add {}, {}", REGS[*dst as usize], REGS[*src as usize]);
            }
            Add(RegImm { reg, val }) => {
                println!("  add {}, {}", REGS[*reg as usize], val);
            }
            AddImm(RegImm { reg, val }) => {
                println!("  mov {}, {}", REGS[*reg as usize], val);
            }
            Sub(RegReg { dst, src }) => {
                println!("  sub {}, {}", REGS[*dst as usize], REGS[*src as usize]);
            }
            Mul(RegReg { dst, src }) => {
                println!("  mov rax, {}", REGS[*src as usize]);
                println!("  mul {}", REGS[*dst as usize]);
                println!("  mov {}, rax", REGS[*dst as usize]);
            }
            Div(RegReg { dst, src }) => {
                println!("  mov rax, {}", REGS[*dst as usize]);
                println!("  cqo");
                println!("  div {}", REGS[*src as usize]);
                println!("  mov {}, rax", REGS[*dst as usize]);
            }
            IR::Call(IRType::Call { reg, name, args }) => {
                // TODO: only support 6 parameters atm
                const P: [&str; 7] = ["rbx", "rbp", "rsp", "r12", "r13", "r14", "r15"];
                for r in P.iter().take(args.len()) {
                    // TODO only push the registers we're using
                    println!("  push {}", r);
                }

                const A: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
                for (i, arg) in args.iter().enumerate() {
                    println!("  mov {}, {}", A[i], REGS[*arg as usize]);
                }

                println!("  mov rax, 0");
                println!("  call {}", name);
                println!("  mov {}, rax", REGS[*reg as usize]);

                for r in P.iter().take(args.len()).collect::<Vec<_>>().iter().rev() {
                    // TODO only push the registers we're using
                    println!("  push {}", r);
                }
            }
            IR::Nop(_) => {}
            ty => fail!("unknown operator: {:?}", ty),
        }
    }

    println!("{}:", ret);
    println!("  mov rsp, rbp");
    println!("  mov rsp, rbp");
    println!("  pop rbp");
    println!("  ret");
}
