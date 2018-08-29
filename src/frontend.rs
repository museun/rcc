use codegen;
use ir::Generate;
use parser::Parser;
use registers::Registers;
use semantics::Semantics;
use tokens::Tokens;

pub enum Error {
    Tokenize,
}

pub fn compile(file: impl AsRef<str>, input: impl AsRef<str>) -> Result<String, Error> {
    let tokens = Tokens::tokenize(file.as_ref(), input.as_ref());
    if tokens.is_empty() {
        return Err(Error::Tokenize {});
    }
    let mut ast = Parser::parse(tokens);
    let ast = Semantics::analyze(&mut ast);
    let mut ir = Generate::generate(&ast);
    let ir = Registers::allocate(&mut ir);
    let asm = codegen::generate_x64(&codegen::ABI::SystemV, ir);
    Ok(asm)
}
