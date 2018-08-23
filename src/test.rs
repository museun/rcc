use std::{
    fs,
    process::Command,
    sync::atomic::{AtomicUsize, Ordering},
};

static POS: AtomicUsize = AtomicUsize::new(1);
// static RUN: AtomicBool = AtomicBool::new(false);

const CC: &str = "bash";

fn clean() {
    // this should only be done once..
    let _ = fs::remove_dir_all("tmp");
    let _ = fs::create_dir("tmp");
}

fn gen_name() -> String {
    format!("{}", POS.fetch_add(1, Ordering::SeqCst))
}

fn compile(p: &str) -> Option<String> {
    let name = gen_name();
    let rcc = Command::new("target/debug/rcc.exe")
        .arg(p)
        .output()
        .expect("to run rcc");

    let out = String::from_utf8_lossy(&rcc.stdout);
    let err = String::from_utf8_lossy(&rcc.stderr);
    if !rcc.status.success() {
        eprintln!();
        for err in err.lines() {
            println!("{}", err);
        }
        eprintln!();
        return None;
    }

    fs::write(&format!("tmp/{}.s", name), &out.into_owned()).expect("to write asm");

    let gcc = Command::new(CC)
        .arg("-c")
        .arg(&format!("gcc -o tmp/{} tmp/{}.s add.o", &name, &name))
        .output()
        .expect("to run gcc");

    let err = String::from_utf8_lossy(&gcc.stderr);
    if !gcc.status.success() {
        eprintln!(
            "failed to run gcc. code {}. {}",
            gcc.status.code().expect("status code"),
            err
        );
        eprintln!("'{}'", p);
        assert!(!gcc.status.success());
    }

    Some(format!("tmp/{}", &name))
}

fn run(fi: impl AsRef<str>) -> i32 {
    let fi = fi.as_ref();
    Command::new(CC)
        .arg("-c")
        .arg(fi)
        .output()
        .expect("to run program")
        .status
        .code()
        .expect("to get status code")
}

#[test]
fn compiler() {
    #[cfg_attr(rustfmt, rustfmt_skip)]
    let tests = &[
        (0, "int main() { return 0; }"),
        (1, "int main() { return 1; }"),
        (42, "int main() { return 42; }"),
        (21, "int main() { return 5+20-4; }"),
        (42, "int main() { return 12 + 35 - 5; }"),
        (36, "int main() { return 1+2+3+4+5+6+7+8; }"),
        (153, "int main() { return 1+2+3+4+5+6+7+8+9+10+11+12+13+14+15+16+17; }"),
        (10, "int main() { return 2*3+4; }"),
        (14, "int main() { return 2+3*4; }"),
        (26, "int main() { return 2*3+4*5; }"),
        (5, "int main() { return 50/10; }"),
        (9, "int main() { return 6*3/2; }"),
        (2, "int main() { int a=2; return a; }"),
        (10, "int main() { int a=2; int b=3+2; return a*b; }"),
        (45, "int main() { return (2+3)*(4+5); }"),
        (8, "int main() { return (2 + (2 * 3)); }"),
        (2, "int main() { if (1) return 2; return 3; }"),
        (3, "int main() { if (0) return 2; return 3; }"),
        (2, "int main() { if (1) return 2; else return 3; }"),
        (3, "int main() { if (0) return 2; else return 3; }"),
        (5, "int main() { return add(2,3); }"),
        (6, "int main() { return add(2*2,3-1); }"),
        (1, "int one() { return 1; } int main() { return one(); }"),
        (3, "int one() { return 1; } int two() { return 2; } int main() { return add(one(), two()); }"),
        (6, "int multiply(a, b) { return a * b; } int main() { return multiply(2, 3); }"),
        (21, "int sum(a,b,c,d,e,f) { return a+b+c+d+e+f; } int main() { return sum(1,2,3,4,5,6); }"),
        (0, "int main() { return 0||0; }"),
        (1, "int main() { return 1||0; }"),
        (1, "int main() { return 0||1; }"),
        (1, "int main() { return 1||1; }"),
        (0, "int main() { return 0&&0; }"),
        (0, "int main() { return 1&&0; }"),
        (0, "int main() { return 0&&1; }"),
        (1, "int main() { return 1&&1; }"),
        (0, "int main() { return 0<0; }"),
        (0, "int main() { return 1<0; }"),
        (1, "int main() { return 0<1; }"),
        (0, "int main() { return 0>0; }"),
        (0, "int main() { return 0>1; }"),
        (1, "int main() { return 1>0; }"),
        (60, "int main() { int sum=0; int i; for (i=10; i<15; i=i+1) sum = sum + i; return sum; }"),
        (89, "int main() { int i=1; int j=1; for (int k=0; k<10; k=k+1) { int m=i+j; i=j; j=m;} return i; }"),        
    ];

    clean();

    let rcc = Command::new("cargo")
        .args(&["build", "--bin", "rcc"])
        .output()
        .expect("build rcc");

    assert_eq!(rcc.status.code(), Some(0));

    for (expected, input) in tests.iter() {
        eprintln!("\x1B[33m=>\x1B[m {}", input);
        eprint!("\x1B[36m??\x1B[m");
        if let Some(program) = compile(input) {
            let actual = run(program);
            if actual == *expected {
                //eprintln!("\x1B[33msource=>\x1B[m {}", p);
                eprintln!("\x1B[1000D\x1B[32mOK:\x1B[m {}", actual);
            } else {
                eprintln!(
                    "\x1B[1000D\x1B[31mFAIL\x1B[m expected {}, got {}",
                    expected, actual
                );
                eprintln!();
                assert_eq!(actual, *expected);
            }
        } else {
            panic!("compiler failed");
        }
    }
}
