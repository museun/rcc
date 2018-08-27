use super::*;
use std::fmt::Write;

const REGS8: [&str; 8] = ["bpl", "r10b", "r11b", "bl", "r12b", "r13b", "r14b", "r15b"];
const REGS32: [&str; 8] = ["ebp", "r10d", "r11d", "ebx", "r12d", "r13d", "r14d", "r15d"];
const REGS64: [&str; 8] = ["rbp", "r10", "r11", "rbx", "r12", "r13", "r14", "r15"];

pub enum ABI {
    Windows,
    SystemV,
}

// this should be a trait
pub fn generate_x64(abi: &ABI, funcs: &[Function]) -> String {
    let mut buf = String::new();
    writeln!(&mut buf, ".intel_syntax noprefix");

    writeln!(&mut buf, ".data");
    for var in funcs.iter().flat_map(|f| &f.globals) {
        if var.is_extern {
            continue;
        }
        if let Some((name, data)) = &var.global {
            writeln!(&mut buf, "{}:", &name);
            writeln!(&mut buf, "  .ascii \"{}\"", escape(&data));
        }
    }

    writeln!(&mut buf, ".text");

    let mut label = 0;
    for func in funcs {
        generate(&mut buf, &abi, &func, &mut label)
    }

    buf
}

fn generate<W: Write>(mut buf: &mut W, abi: &ABI, func: &Function, label: &mut u32) {
    let ret = format!(".Lend{}", label);
    *label += 1;

    writeln!(buf, ".global {}", func.name);
    writeln!(buf, "{}:", func.name);

    writeln!(buf, "  push rbp");
    writeln!(buf, "  mov rbp, rsp");
    writeln!(buf, "  sub rsp, {}", func.stacksize);

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
        writeln!(buf, "  push {}", r);
    }

    use IRType::*;

    for ir in &func.ir {
        match ir {
            IR::Imm(RegImm { reg, val }) => {
                writeln!(buf, "  mov {}, {}", REGS64[*reg as usize], val);
            }

            IR::Mov(RegReg { dst, src }) => {
                writeln!(
                    buf,
                    "  mov {}, {}",
                    REGS64[*dst as usize], REGS64[*src as usize]
                );
            }

            IR::Return(Reg { src }) => {
                writeln!(buf, "  mov rax, {}", REGS64[*src as usize]);
                writeln!(buf, "  jmp {}", ret);
            }

            IR::Load(w, RegReg { dst, src }) => {
                writeln!(
                    buf,
                    "  mov {}, [{}]",
                    match w {
                        Width::W8 => REGS8[*dst as usize],
                        Width::W32 => REGS32[*dst as usize],
                        Width::W64 => REGS64[*dst as usize],
                    },
                    REGS64[*src as usize]
                );

                if let Width::W8 = w {
                    writeln!(
                        buf,
                        "  movzx {}, {}",
                        REGS64[*dst as usize], REGS8[*dst as usize]
                    );
                }
            }

            IR::Store(w, RegReg { dst, src }) => {
                writeln!(
                    buf,
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
                writeln!(
                    buf,
                    "  mov [rbp-{}], {}",
                    val,
                    match w {
                        Width::W8 => caller8[*reg as usize],
                        Width::W32 => caller32[*reg as usize],
                        Width::W64 => caller64[*reg as usize],
                    },
                );
            }

            IR::Label(IRType::Imm { val }) => {
                writeln!(buf, ".L{}:", val);
            }
            IR::Label(IRType::RegLabel { reg, label }) => {
                writeln!(buf, "  lea {}, {}", REGS64[*reg as usize], label);
            }

            IR::Unless(RegImm { reg, val }) => {
                writeln!(buf, "  cmp {}, 0", REGS64[*reg as usize]);
                writeln!(buf, "  je .L{}", val);
            }

            IR::Jmp(IRType::Imm { val }) => {
                writeln!(buf, "  jmp .L{}", val);
            }

            IR::If(IRType::RegImm { reg, val }) => {
                writeln!(buf, "  cmp {}, 0", REGS64[*reg as usize]);
                writeln!(buf, "  jne .L{}", val);
            }

            IR::Add(RegReg { dst, src }) => {
                writeln!(
                    buf,
                    "  add {}, {}",
                    REGS64[*dst as usize], REGS64[*src as usize]
                );
            }

            IR::Add(RegImm { reg, val }) => {
                writeln!(buf, "  mov {}, {}", REGS64[*reg as usize], val);
            }

            IR::Sub(RegReg { dst, src }) => {
                writeln!(
                    buf,
                    "  sub {}, {}",
                    REGS64[*dst as usize], REGS64[*src as usize]
                );
            }

            IR::Sub(RegImm { reg, val }) => {
                writeln!(buf, "  sub {}, {}", REGS64[*reg as usize], val);
            }

            IR::Mul(RegReg { dst, src }) => {
                writeln!(buf, "  mov rax, {}", REGS64[*src as usize]);
                writeln!(buf, "  mul {}", REGS64[*dst as usize]);
                writeln!(buf, "  mov {}, rax", REGS64[*dst as usize]);
            }

            IR::Div(RegReg { dst, src }) => {
                writeln!(buf, "  mov rax, {}", REGS64[*dst as usize]);
                writeln!(buf, "  cqo");
                writeln!(buf, "  div {}", REGS64[*src as usize]);
                writeln!(buf, "  mov {}, rax", REGS64[*dst as usize]);
            }

            IR::Comparison(Cmp { .. }) => emit_cmp(&mut buf, &ir),

            IR::Call(IRType::Call { reg, name, args }) => {
                for (i, arg) in args.iter().enumerate() {
                    writeln!(buf, "  mov {}, {}", caller64[i], REGS64[*arg as usize]);
                }

                writeln!(buf, "  push r10");
                writeln!(buf, "  push r11");
                writeln!(buf, "  mov rax, 0");
                writeln!(buf, "  call {}", name);
                writeln!(buf, "  pop r11");
                writeln!(buf, "  pop r10");

                writeln!(buf, "  mov {}, rax", REGS64[*reg as usize]);
            }

            IR::Nop(_) => {}
            ty => fail!("unknown operator: {:?}", ty),
        }
    }

    writeln!(buf, "{}:", ret);
    if let ABI::SystemV = abi {
        for r in callee.iter().rev() {
            writeln!(buf, "  pop {}", r);
        }
    }
    writeln!(buf, "  mov rsp, rbp");
    writeln!(buf, "  pop rbp");
    writeln!(buf, "  ret");
}

fn emit_cmp<W: Write>(mut buf: W, ir: &IR) {
    if let IR::Comparison(IRType::Cmp { cmp, dst, src }) = &ir {
        writeln!(
            buf,
            "  cmp {}, {}",
            REGS64[*dst as usize], REGS64[*src as usize]
        );
        let _ = match cmp {
            Cmp::Lt | Cmp::Gt => writeln!(buf, "  setl {}", REGS8[*dst as usize]),
            Cmp::Eq => writeln!(buf, "  sete {}", REGS8[*dst as usize]),
            Cmp::NEq => writeln!(buf, "  setne {}", REGS8[*dst as usize]),
        };
        writeln!(
            buf,
            "  movzx {}, {}",
            REGS64[*dst as usize], REGS8[*dst as usize]
        );
    } else {
        unreachable!();
    }
}

fn escape(s: &str) -> String {
    if s.is_empty() {
        return escape("\0");
    }

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
