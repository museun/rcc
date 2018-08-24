use super::*;

pub enum ABI {
    Windows,
    SystemV,
}

// this should be a trait
pub fn generate_x64(abi: &ABI, funcs: &[Function]) {
    println!(".intel_syntax noprefix");
    let mut label = 0;
    for func in funcs {
        generate(&abi, &func, &mut label)
    }
}

fn generate(abi: &ABI, func: &Function, label: &mut u32) {
    let ret = format!(".Lend{}", label);
    *label += 1;

    println!(".global {}", func.name);
    println!("{}:", func.name);

    println!("  push rbp");
    println!("  mov rbp, rsp");
    println!("  sub rsp, {}", func.stacksize);

    // windows uses RAX RCX RDX R8 R9 R10 11
    // caller saved
    // sys-v uses RDI RSI RDX RC9 R8 R9
    // caller saved

    // windows uses RDI RSI RSP R12 R13 R14 R15
    // these are callee saved
    // sys-v uses R12 R13 R14 R15
    // sys-v must restore original values

    let (callee, caller) = match abi {
        ABI::Windows => unimplemented!(),
        ABI::SystemV => (
            &["rdi", "rsi", "rdx", "rcx", "r8", "r9"], // caller
            &["r12", "r13", "r14", "r15"],             // callee
        ),
    };

    for r in caller {
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
                println!("  mov {}, {}", REGS[*reg as usize], val);
            }
            IR::Sub(RegReg { dst, src }) => {
                println!("  sub {}, {}", REGS[*dst as usize], REGS[*src as usize]);
            }
            IR::Sub(RegImm { reg, val }) => {
                println!("  sub {}, {}", REGS[*reg as usize], val);
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
            IR::Comparison(RegReg { dst, src }) => {
                println!("  cmp {}, {}", REGS[*dst as usize], REGS[*src as usize]);
                println!("  setl {}", REGS8[*dst as usize]);
                println!("  movzb {}, {}", REGS[*dst as usize], REGS8[*dst as usize]);
            }
            IR::SaveArgs(Imm { val }) => {
                for i in 0..*val {
                    println!("  mov [rbp-{}], {}", (i + 1) * 8, callee[i as usize]);
                }
            }
            IR::Call(IRType::Call { reg, name, args }) => {
                for (i, arg) in args.iter().enumerate() {
                    println!("  mov {}, {}", callee[i], REGS[*arg as usize]);
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
    if let ABI::SystemV = abi {
        for r in caller.iter().rev() {
            println!("  pop {}", r);
        }
    }
    println!("  mov rsp, rbp");
    println!("  pop rbp");
    println!("  ret");
}
