#![allow(dead_code)]
use super::*;
use std::{fs, process::Command};

// TODO use proper Paths
// TODO make tests and tmp

fn compile(n: usize, p: &str) -> Option<String> {
    let program = frontend::compile(p).ok()?;

    let file = format!("test_{:04}", n);
    fs::write(&format!("tmp/{}.s", file), &program).expect("to write asm");

    let cc = Command::new("bash")
        .arg("-c")
        .arg(&format!(
            "clang -static -o tmp/{} tests/test.o tmp/{}.s",
            &file, &file
        )).output()
        .expect("to run cc");

    let err = String::from_utf8_lossy(&cc.stderr);
    if !cc.status.success() {
        panic!(
            "failed to run CC. code {}. {}",
            cc.status.code().expect("status code"),
            err
        );
    }

    Some(format!("tmp/{}", file))
}

fn run(fi: impl AsRef<str>) -> isize {
    let fi = fi.as_ref();
    eprintln!("running: {}", fi);
    Command::new("bash")
        .arg("-c")
        .arg(fi)
        .output()
        .expect("to run program")
        .status
        .code()
        .expect("to get status code") as isize
}

fn expect(expected: isize, actual: isize) {
    assert_eq!(expected, actual, "expected: {}, got: {}", expected, actual);
}

include!(concat!(env!("OUT_DIR"), "/tests.rs"));
