use super::*;

// this should be a trait
pub fn generate_x86(funcs: Vec<Function>) {
    println!(".intel_syntax noprefix");
    let mut label = 0;
    for func in funcs {
        generate(&func, &mut label)
    }
}

fn generate(func: &Function, label: &mut u32) {
    let ret = format!(".Lend{}", label);
    *label += 1;

    println!(".global {}", func.name);
    println!("{}:", func.name);

    println!("  push rbp");
    println!("  mov rbp, rsp");
    println!("  sub rsp, {}", func.stacksize);

    const R: [&str; 4] = ["r12", "r13", "r14", "r15"];
    for r in &R {
        println!("  push {}", r);
    }

    use IRType::*;
    for ir in &func.ir {
        match &ir {
            IR::Imm(RegImm { reg, val }) => {
                println!("  mov {}, {}", REGS[*reg as usize], val);
            }
            IR::Mov(RegReg { dst, src }) => {
                println!("  mov {}, {}", REGS[*dst as usize], REGS[*src as usize]);
            }
            IR::Return(Reg { src }) => {
                println!("  mov rax, {}", REGS[*src as usize]);
                println!("  jmp {}", ret);
            }
            IR::Load(RegReg { dst, src }) => {
                println!("  mov {}, [{}]", REGS[*dst as usize], REGS[*src as usize]);
            }
            IR::Store(RegReg { dst, src }) => {
                println!("  mov [{}], {}", REGS[*dst as usize], REGS[*src as usize]);
            }
            IR::Label(IRType::Imm { val }) => println!(".L{}:", val),
            IR::Unless(RegImm { reg, val }) => {
                println!("  cmp {}, 0", REGS[*reg as usize]);
                println!("  je .L{}", val)
            }
            IR::Jmp(IRType::Imm { val }) => println!("  jmp .L{}", val),
            IR::Add(RegReg { dst, src }) => {
                println!("  add {}, {}", REGS[*dst as usize], REGS[*src as usize]);
            }
            IR::Add(RegImm { reg, val }) => {
                println!("  add {}, {}", REGS[*reg as usize], val);
            }
            IR::AddImm(RegImm { reg, val }) => {
                println!("  mov {}, {}", REGS[*reg as usize], val);
            }
            IR::Sub(RegReg { dst, src }) => {
                println!("  sub {}, {}", REGS[*dst as usize], REGS[*src as usize]);
            }
            IR::Mul(RegReg { dst, src }) => {
                println!("  mov rax, {}", REGS[*src as usize]);
                println!("  mul {}", REGS[*dst as usize]);
                println!("  mov {}, rax", REGS[*dst as usize]);
            }
            IR::Div(RegReg { dst, src }) => {
                println!("  mov rax, {}", REGS[*dst as usize]);
                println!("  cqo");
                println!("  div {}", REGS[*src as usize]);
                println!("  mov {}, rax", REGS[*dst as usize]);
            }
            IR::Call(IRType::Call { reg, name, args }) => {
                const A: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
                for (i, arg) in args.iter().enumerate() {
                    println!("  mov {}, {}", A[i], REGS[*arg as usize]);
                }

                println!("  push r10");
                println!("  push r11");
                println!("  mov rax, 0");
                println!("  call {}", name);
                println!("  pop r11");
                println!("  pop r10");

                println!("  mov {}, rax", REGS[*reg as usize]);
            }
            IR::Nop(_) => {}
            ty => fail!("unknown operator: {:?}", ty),
        }
    }

    println!("{}:", ret);
    for r in R.iter().rev() {
        println!("  pop {}", r);
    }
    println!("  mov rsp, rbp");
    println!("  pop rbp");
    println!("  ret");
}
