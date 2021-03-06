use super::*;
use ir::Function;
use node::Comp;

use std::fmt::Write;

pub const REGS8: [&str; 7] = ["r10b", "r11b", "bl", "r12b", "r13b", "r14b", "r15b"];
pub const REGS32: [&str; 7] = ["r10d", "r11d", "ebx", "r12d", "r13d", "r14d", "r15d"];
pub const REGS64: [&str; 7] = ["r10", "r11", "rbx", "r12", "r13", "r14", "r15"];

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
    writeln!(buf, "  sub rsp, {}", round(func.stacksize, 16));

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

    use ir::IRType::*;
    use ir::*;

    for ir in &func.ir {
        match (&ir.kind, &ir.ty) {
            (IRKind::Imm, RegImm { reg, val }) => {
                writeln!(buf, "  mov {}, {}", REGS64[*reg as usize], val);
            }

            (IRKind::Mov, RegReg { dst, src }) => {
                writeln!(
                    buf,
                    "  mov {}, {}",
                    REGS64[*dst as usize], REGS64[*src as usize]
                );
            }

            (IRKind::Return, Reg { src }) => {
                writeln!(buf, "  mov rax, {}", REGS64[*src as usize]);
                writeln!(buf, "  jmp {}", ret);
            }

            (IRKind::Load(w), RegReg { dst, src }) => {
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

            (IRKind::Store(w), RegReg { dst, src }) => {
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

            (IRKind::StoreArg(w), RegImm { reg, val }) => {
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

            (IRKind::Label, IRType::Imm { val }) => {
                writeln!(buf, ".L{}:", val);
            }

            (IRKind::Label, IRType::RegLabel { reg, label }) => {
                writeln!(buf, "  lea {}, {}", REGS64[*reg as usize], label);
            }

            (IRKind::Unless, RegImm { reg, val }) => {
                writeln!(buf, "  cmp {}, 0", REGS64[*reg as usize]);
                writeln!(buf, "  je .L{}", val);
            }

            (IRKind::Jmp, IRType::Imm { val }) => {
                writeln!(buf, "  jmp .L{}", val);
            }

            (IRKind::Or, IRType::RegReg { dst, src }) => {
                writeln!(
                    buf,
                    "  or {},{}",
                    REGS64[*dst as usize], REGS64[*src as usize]
                );
            }

            (IRKind::Xor, IRType::RegReg { dst, src }) => {
                writeln!(
                    buf,
                    "  xor {},{}",
                    REGS64[*dst as usize], REGS64[*src as usize]
                );
            }

            (IRKind::Xor, IRType::RegImm { reg, val }) => {
                writeln!(
                    buf,
                    "  xor {},{}",
                    REGS64[*reg as usize], val
                );
            }

            (IRKind::And, IRType::RegReg { dst, src }) => {
                writeln!(
                    buf,
                    "  and {},{}",
                    REGS64[*dst as usize], REGS64[*src as usize]
                );
            }

            (IRKind::If, IRType::RegImm { reg, val }) => {
                writeln!(buf, "  cmp {}, 0", REGS64[*reg as usize]);
                writeln!(buf, "  jne .L{}", val);
            }

            (IRKind::BpRel, RegImm { reg, val }) => {
                writeln!(buf, "  lea {}, [rbp-{}]", REGS64[*reg as usize], val);
            }

            (IRKind::Add, RegReg { dst, src }) => {
                writeln!(
                    buf,
                    "  add {}, {}",
                    REGS64[*dst as usize], REGS64[*src as usize]
                );
            }

            (IRKind::Add, RegImm { reg, val }) => {
                writeln!(buf, "  add {}, {}", REGS64[*reg as usize], val);
            }

            (IRKind::Sub, RegReg { dst, src }) => {
                writeln!(
                    buf,
                    "  sub {}, {}",
                    REGS64[*dst as usize], REGS64[*src as usize]
                );
            }

            (IRKind::Sub, RegImm { reg, val }) => {
                writeln!(buf, "  sub {}, {}", REGS64[*reg as usize], val);
            }

            (IRKind::Mul, RegReg { dst, src }) => {
                // this could just be a SHL if src < 256 && src's 1s == 1
                writeln!(buf, "  mov rax, {}", REGS64[*src as usize]);
                writeln!(buf, "  mul {}", REGS64[*dst as usize]);
                writeln!(buf, "  mov {}, rax", REGS64[*dst as usize]);
            }

            (IRKind::Mul, RegImm { reg, val }) => {
                writeln!(buf, "  mov rax, {}", val);
                writeln!(buf, "  mul {}", REGS64[*reg as usize]);
                writeln!(buf, "  mov {}, rax", REGS64[*reg as usize]);
            }

            (IRKind::Div, RegReg { dst, src }) => {
                writeln!(buf, "  mov rax, {}", REGS64[*dst as usize]);
                writeln!(buf, "  cqo");
                writeln!(buf, "  div {}", REGS64[*src as usize]);
                writeln!(buf, "  mov {}, rax", REGS64[*dst as usize]);
            }

            (IRKind::Mod, RegReg { dst, src }) => {
                writeln!(buf, "  mov rax, {}", REGS64[*dst as usize]);
                writeln!(buf, "  cqo");
                writeln!(buf, "  div {}", REGS64[*src as usize]);
                writeln!(buf, "  mov {}, rdx", REGS64[*dst as usize]);
            }

            (IRKind::Shl, RegReg { dst, src }) => {
                writeln!(buf, "  mov cl, {}", REGS8[*src as usize]);
                writeln!(buf, "  shl {}, cl", REGS64[*dst as usize]);
            }

            (IRKind::Shr, RegReg { dst, src }) => {
                writeln!(buf, "  mov cl, {}", REGS8[*src as usize]);
                writeln!(buf, "  shr {}, cl", REGS64[*dst as usize]);
            }

            (IRKind::Neg, Reg { src }) => {
                writeln!(buf, "  neg {}", REGS64[*src as usize]);
            }

            (IRKind::Comparison, Cmp { dst, src, ref cmp }) => emit_cmp(&mut buf, cmp, *dst, *src),

            (IRKind::Call, IRType::Call { reg, name, args }) => {
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

            (IRKind::Nop, _) => {}

            _ => fail!("unknown operator: {}", ir),
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

fn emit_cmp<W: Write>(mut buf: W, cmp: &Comp, dst: i32, src: i32) {
    writeln!(
        buf,
        "  cmp {}, {}",
        REGS64[dst as usize], REGS64[src as usize]
    );
    let _ = match cmp {
        Comp::LessThan | Comp::GreaterThan => writeln!(buf, "  setl {}", REGS8[dst as usize]),
        Comp::LessThanEq | Comp::GreaterThanEq => writeln!(buf, "  setle {}", REGS8[dst as usize]),
        Comp::Equal => writeln!(buf, "  sete {}", REGS8[dst as usize]),
        Comp::NotEqual => writeln!(buf, "  setne {}", REGS8[dst as usize]),
    };
    writeln!(
        buf,
        "  movzx {}, {}",
        REGS64[dst as usize], REGS8[dst as usize]
    );
}

fn escape(s: &str) -> String {
    if s.is_empty() {
        return escape("\0");
    }

    s.replace('\\', "\\")
        .chars()
        .map(|c| {
            if c.is_control() {
                format!("\\{:03o}", c as u8) // HACK: utf-8 strings in rust
            } else {
                format!("{}", c)
            }
        }).collect()
}
