#![allow(unused_imports)]
use std::{
    fs,
    process::Command,
    sync::atomic::{AtomicUsize, Ordering},
};

#[cfg(test)]
static POS: AtomicUsize = AtomicUsize::new(1);

#[cfg(test)]
const CC: &str = "bash";

#[cfg(test)]
fn clean() {
    // this should only be done once..
    let _ = fs::remove_dir_all("tmp");
    let _ = fs::create_dir("tmp");
}

#[cfg(test)]
fn gen_name() -> String {
    format!("{}", POS.fetch_add(1, Ordering::SeqCst))
}

#[cfg(test)]
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
        .arg(&format!("gcc -o tmp/{} tmp/{}.s test.o", &name, &name))
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

#[cfg(test)]
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

#[cfg_attr(rustfmt, rustfmt_skip)]
pub const TESTS: &[(usize, &str)] = &[
/*  0 */    (0, "int main() { return 0; }"),
/*  1 */    (1, "int main() { return 1; }"),
/*  2 */    (42, "int main() { return 42; }"),
/*  3 */    (21, "int main() { return 5+20-4; }"),
/*  4 */    (42, "int main() { return 12 + 35 - 5; }"),
/*  5 */    (36, "int main() { return 1+2+3+4+5+6+7+8; }"),
/*  6 */    (153, "int main() { return 1+2+3+4+5+6+7+8+9+10+11+12+13+14+15+16+17; }"),
/*  7 */    (10, "int main() { return 2*3+4; }"),
/*  8 */    (14, "int main() { return 2+3*4; }"),
/*  9 */    (26, "int main() { return 2*3+4*5; }"),
/* 10 */    (5, "int main() { return 50/10; }"),
/* 11 */    (9, "int main() { return 6*3/2; }"),
/* 12 */    (2, "int main() { int a=2; return a; }"),
/* 13 */    (10, "int main() { int a=2; int b=3+2; return a*b; }"),
/* 14 */    (45, "int main() { return (2+3)*(4+5); }"),
/* 15 */    (8, "int main() { return (2 + (2 * 3)); }"),
/* 16 */    (2, "int main() { if (1) return 2; return 3; }"),
/* 17 */    (3, "int main() { if (0) return 2; return 3; }"),
/* 18 */    (2, "int main() { if (1) return 2; else return 3; }"),
/* 19 */    (3, "int main() { if (0) return 2; else return 3; }"),
/* 20 */    (5, "int main() { return add(2,3); }"),
/* 21 */    (6, "int main() { return add(2*2,3-1); }"),
/* 22 */    (1, "int one() { return 1; } int main() { return one(); }"),
/* 23 */    (3, "int one() { return 1; } int two() { return 2; } int main() { return add(one(), two()); }"),    
/* 24 */    (6, "int multiply(int a, int b) { return a * b; } int main() { return multiply(2, 3); }"), 
/* 25 */    (21, "int sum(int a,int b,int c,int d,int e,int f) { return a+b+c+d+e+f; } int main() { return sum(1,2,3,4,5,6); }"),
/* 26 */    (0, "int main() { return 0||0; }"),
/* 27 */    (1, "int main() { return 1||0; }"),
/* 28 */    (1, "int main() { return 0||1; }"),
/* 29 */    (1, "int main() { return 1||1; }"),
/* 30 */    (0, "int main() { return 0&&0; }"),
/* 31 */    (0, "int main() { return 1&&0; }"),
/* 32 */    (0, "int main() { return 0&&1; }"),
/* 33 */    (1, "int main() { return 1&&1; }"),
/* 34 */    (0, "int main() { return 0<0; }"),
/* 35 */    (0, "int main() { return 1<0; }"),
/* 36 */    (1, "int main() { return 0<1; }"),
/* 37 */    (0, "int main() { return 0>0; }"),
/* 38 */    (0, "int main() { return 0>1; }"),
/* 39 */    (1, "int main() { return 1>0; }"),
/* 40 */    (60, "int main() { int sum=0; int i; for (i=10; i<15; i=i+1) sum = sum + i; return sum; }"),
/* 41 */    (89, "int main() { int i=1; int j=1; for (int k=0; k<10; k=k+1) { int m=i+j; i=j; j=m;} return i; }"),
/* 42 */    (42, "int main() { int *p = alloc(42); return *p; }"),
/* 43 */    (8, "int main() { int *p = alloc_pair(3, 5); return *p + *(p + 1); }"),
/* 44 */    (9, "int main() { int *p = alloc_offset(2, 7); return *p + *(p - 1); }"),
/* 45 */    (42, "int main() { int **p = alloc_pointer(42); return **p; }"),
/* 46 */    (3, "int main() { int arr[2]; *arr=1; *(arr+1)=2; return *arr + *(arr + 1); }"),
/* 47 */    (5, "int main() { int x; int *p = &x; x = 5; return *p; }"),
/* 48 */    (3, "int main() { int arr[2]; arr[0]=1; arr[1]=2; return arr[0] + arr[1]; }"),
/* 49 */    (5, "int main() { int x; int *p = &x; x = 5; return p[0]; }"),
/* 50 */    (4, "int main() { int x; return sizeof(x); }"),
/* 51 */    (8, "int main() { int *x; return sizeof x; }"),
/* 52 */    (16, "int main() { int x[4]; return sizeof x; }"),
];

#[test]
fn compiler() {
    clean();

    let rcc = Command::new("cargo")
        .args(&["build", "--bin", "rcc"])
        .output()
        .expect("build rcc");

    assert_eq!(rcc.status.code(), Some(0));

    for (i, (expected, input)) in TESTS.iter().enumerate() {
        eprintln!("\x1B[33m=>\x1B[m {}", input);
        eprint!("\x1B[36m??: {}\x1B[m", i);
        if let Some(program) = compile(input) {
            let actual = run(program);
            if actual == *expected as i32 {
                eprintln!("\x1B[1000D\x1B[32mOK:\x1B[m {}          ", actual);
            } else {
                eprintln!(
                    "\x1B[1000D\x1B[31mFAIL\x1B[m #{}, expected {}, got {}",
                    i, expected, actual
                );
                eprintln!();
                assert_eq!(actual, *expected as i32);
            }
        } else {
            panic!("compiler failed");
        }
    }
}
