use super::*;

const REGS8: [&str; 8] = ["bpl", "r10b", "r11b", "bl", "r12b", "r13b", "r14b", "r15b"];
const REGS32: [&str; 8] = ["ebp", "r10d", "r11d", "ebx", "r12d", "r13d", "r14d", "r15d"];
const REGS64: [&str; 8] = ["rbp", "r10", "r11", "rbx", "r12", "r13", "r14", "r15"];

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
    println!(".data");
    for var in &func.globals {
        if let Some((name, data)) = &var.global {
            println!("{}:", &name);
            println!("  .ascii \"{}\"", escape(&data));
        }
    }

    let ret = format!(".Lend{}", label);
    *label += 1;

    println!(".text");
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

    let (caller8, caller32, caller64, callee) = match abi {
        ABI::Windows => unimplemented!(),
        ABI::SystemV => (
            &["dil", "sil", "dl", "cl", "r8b", "r9b"],   // caller 8
            &["edi", "esi", "edx", "ecx", "r8d", "r9d"], // caller 32
            &["rdi", "rsi", "rdx", "rcx", "r8", "r9"],   // caller 64
            &["r12", "r13", "r14", "r15"],               // callee
        ),
    };

    for r in callee {
        println!("  push {}", r);
    }

    use IRType::*;
    for ir in &func.ir {
        match &ir {
            IR::Imm(RegImm { reg, val }) => {
                println!("  mov {}, {}", REGS64[*reg as usize], val);
            }

            IR::Mov(RegReg { dst, src }) => {
                println!("  mov {}, {}", REGS64[*dst as usize], REGS64[*src as usize]);
            }

            IR::Return(Reg { src }) => {
                println!("  mov rax, {}", REGS64[*src as usize]);
                println!("  jmp {}", ret);
            }

            IR::Load(w, RegReg { dst, src }) => {
                println!(
                    "  mov {}, [{}]",
                    match w {
                        Width::W8 => REGS8[*dst as usize],
                        Width::W32 => REGS32[*dst as usize],
                        Width::W64 => REGS64[*dst as usize],
                    },
                    REGS64[*src as usize]
                );

                if let Width::W8 = w {
                    println!(
                        "  movzx {}, {}",
                        REGS64[*dst as usize], REGS8[*dst as usize]
                    )
                }
            }

            IR::Store(w, RegReg { dst, src }) => {
                println!(
                    "  mov [{}], {}",
                    REGS64[*dst as usize],
                    match w {
                        Width::W8 => REGS8[*src as usize],
                        Width::W32 => REGS32[*src as usize],
                        Width::W64 => REGS64[*src as usize],
                    },
                );
            }

            IR::StoreArg(w, RegImm { reg, val }) => {
                println!(
                    "  mov [rbp-{}], {}",
                    val,
                    match w {
                        Width::W8 => caller8[*reg as usize],
                        Width::W32 => caller32[*reg as usize],
                        Width::W64 => caller64[*reg as usize],
                    },
                );
            }

            IR::Label(IRType::Imm { val }) => println!(".L{}:", val),
            IR::Label(IRType::RegLabel { reg, label }) => {
                println!("  lea {}, {}", REGS64[*reg as usize], label)
            }

            IR::Unless(RegImm { reg, val }) => {
                println!("  cmp {}, 0", REGS64[*reg as usize]);
                println!("  je .L{}", val)
            }

            IR::Jmp(IRType::Imm { val }) => println!("  jmp .L{}", val),

            IR::Add(RegReg { dst, src }) => {
                println!("  add {}, {}", REGS64[*dst as usize], REGS64[*src as usize]);
            }

            IR::Add(RegImm { reg, val }) => {
                println!("  mov {}, {}", REGS64[*reg as usize], val);
            }

            IR::Sub(RegReg { dst, src }) => {
                println!("  sub {}, {}", REGS64[*dst as usize], REGS64[*src as usize]);
            }

            IR::Sub(RegImm { reg, val }) => {
                println!("  sub {}, {}", REGS64[*reg as usize], val);
            }

            IR::Mul(RegReg { dst, src }) => {
                println!("  mov rax, {}", REGS64[*src as usize]);
                println!("  mul {}", REGS64[*dst as usize]);
                println!("  mov {}, rax", REGS64[*dst as usize]);
            }

            IR::Div(RegReg { dst, src }) => {
                println!("  mov rax, {}", REGS64[*dst as usize]);
                println!("  cqo");
                println!("  div {}", REGS64[*src as usize]);
                println!("  mov {}, rax", REGS64[*dst as usize]);
            }

            IR::Comparison(RegReg { dst, src }) => {
                println!("  cmp {}, {}", REGS64[*dst as usize], REGS64[*src as usize]);
                println!("  setl {}", REGS8[*dst as usize]);
                println!(
                    "  movzx {}, {}",
                    REGS64[*dst as usize], REGS8[*dst as usize]
                );
            }

            IR::Call(IRType::Call { reg, name, args }) => {
                for (i, arg) in args.iter().enumerate() {
                    println!("  mov {}, {}", caller64[i], REGS64[*arg as usize]);
                }

                println!("  push r10");
                println!("  push r11");
                println!("  mov rax, 0");
                println!("  call {}", name);
                println!("  pop r11");
                println!("  pop r10");

                println!("  mov {}, rax", REGS64[*reg as usize]);
            }

            IR::Nop(_) => {}

            ty => fail!("unknown operator: {:?}", ty),
        }
    }

    println!("{}:", ret);
    if let ABI::SystemV = abi {
        for r in callee.iter().rev() {
            println!("  pop {}", r);
        }
    }
    println!("  mov rsp, rbp");
    println!("  pop rbp");
    println!("  ret");
}

fn escape(s: &str) -> String {
    s.replace('\\', "\\\\")
        .chars()
        .map(|c| {
            if c.is_control() {
                format!("\\{:03o}", c as u8) // HACK: utf-8 strings in rust
            } else {
                format!("{}", c)
            }
        }).collect()
}
