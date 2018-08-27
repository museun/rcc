use super::*;

pub enum Error {
    Tokenize,
}

pub fn compile(input: impl AsRef<str>) -> Result<String, Error> {
    let tokens = Tokens::tokenize(input.as_ref());
    if tokens.is_empty() {
        return Err(Error::Tokenize {});
    }
    let mut ast = Node::parse(tokens);
    let ast = Semantics::analyze(&mut ast);
    let mut ir = Generate::gen_ir(&ast);
    let ir = Registers::allocate(&mut ir);
    let asm = generate_x64(&ABI::SystemV, ir);
    Ok(asm)
}
