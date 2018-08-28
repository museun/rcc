#![allow(dead_code, unused_imports)]
use std::{
    env,
    fs::{self, File},
    io::Write,
    path::Path,
    process::Command,
};

#[cfg(feature = "testing")]
fn main() {
    if let Ok(s) = env::var("RCC_SKIP_PRECOMPILE") {
        if s == "1" {
            return;
        }
    }

    compile_shared();
    let out_dir = env::var("OUT_DIR").unwrap();
    let test_file = Path::new(&out_dir).join("tests.rs");
    let mut f = File::create(&test_file).unwrap();

    for n in 1.. {
        if let Some(src) = get_source_for(n) {
            let header = src.lines().next().expect("to get first line");
            let expected = header[11..].parse::<isize>().expect("to get result");
            generate_test(&mut f, n, expected, &src);
        } else {
            break;
        }
    }
}

fn generate_test<W: Write>(mut f: W, n: usize, expected: isize, source: &str) {
    writeln!(
        f,
        r###"#[test]
fn test_{:04}() {{
    let source = r#"
{}
    "#;

    expect({}, run(compile("tests/test_{:04}.c", {}, &source).expect("to compile")));
}}
    "###,
        n, source, expected, n, n
    );
}

fn compile_shared() {
    let cc = Command::new("bash")
        .arg("-c")
        .arg("clang -static -c -o tests/test.o test.c")
        .output()
        .expect("to run cc");

    let err = String::from_utf8_lossy(&cc.stderr);
    if !cc.status.success() {
        eprintln!(
            "failed to run CC. code {}. {}",
            cc.status.code().expect("status code"),
            err
        );
        assert!(!cc.status.success());
    }
}

fn get_source_for(n: usize) -> Option<String> {
    fs::read_to_string(&format!("tests/test_{:04}.c", n)).ok()
}

#[cfg(not(feature = "testing"))]
fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let test_file = Path::new(&out_dir).join("tests.rs");
    let mut f = File::create(&test_file).unwrap();
    write!(f, "{}", "// nothing to see here");
}
