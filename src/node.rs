use super::*;
use kind::Kind;
use types::Var;

use std::fmt;

#[allow(unknown_lints, large_enum_variant)]
#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Constant {
        val: u32, // XXX: why is this a u32
        ty: RefType,
    },

    Ident {
        name: String,
    },

    Str {
        str: String,
        ty: RefType, // array
    },

    Add {
        lhs: Kind,
        rhs: Kind,
    },

    Sub {
        lhs: Kind,
        rhs: Kind,
    },

    Mul {
        lhs: Kind,
        rhs: Kind,
    },

    Div {
        lhs: Kind,
        rhs: Kind,
    },

    Not {
        expr: Kind,
    },

    Or {
        lhs: Kind,
        rhs: Kind,
    },

    Xor {
        lhs: Kind,
        rhs: Kind,
    },

    Mod {
        lhs: Kind,
        rhs: Kind,
    },

    Shr {
        lhs: Kind,
        rhs: Kind,
    },

    Shl {
        lhs: Kind,
        rhs: Kind,
    },

    And {
        lhs: Kind,
        rhs: Kind,
    },

    LogAnd {
        lhs: Kind,
        rhs: Kind,
    },

    LogOr {
        lhs: Kind,
        rhs: Kind,
    },

    PostInc {
        expr: Kind,
    },

    PostDec {
        expr: Kind,
    },

    PreInc {
        expr: Kind,
    },

    PreDec {
        expr: Kind,
    },

    Comparison {
        lhs: Kind,
        rhs: Kind,
        comp: Comp,
    },

    Assign {
        lhs: Kind,
        rhs: Kind,
    },

    MulAssign {
        lhs: Kind,
        rhs: Kind,
    },

    DivAssign {
        lhs: Kind,
        rhs: Kind,
    },

    ModAssign {
        lhs: Kind,
        rhs: Kind,
    },

    AddAssign {
        lhs: Kind,
        rhs: Kind,
    },

    SubAssign {
        lhs: Kind,
        rhs: Kind,
    },

    AndAssign {
        lhs: Kind,
        rhs: Kind,
    },

    XorAssign {
        lhs: Kind,
        rhs: Kind,
    },

    OrAssign {
        lhs: Kind,
        rhs: Kind,
    },

    ShlAssign {
        lhs: Kind,
        rhs: Kind,
    },

    ShrAssign {
        lhs: Kind,
        rhs: Kind,
    },

    LVal {
        offset: i32,
        ty: RefType,
    },

    GVar {
        name: String,
        ty: RefType,
    },

    Neg {
        expr: Kind,
    },

    Addr {
        expr: Kind,
        ty: RefType,
    },

    Deref {
        expr: Kind,
    },

    Vardef {
        ty: RefType,
        name: String,
        init: Kind,
        offset: i32,
        data: i32,
        is_extern: bool,
    },

    Struct {
        members: Vec<Kind>,
        offset: i32, // used for alignment
    },

    Dot {
        expr: Kind,
        name: String,
        offset: i32, // used for offset into the struct
    },

    Return {
        expr: Kind,
    },

    Sizeof {
        expr: Kind,
    },

    Alignof {
        expr: Kind,
    },

    If {
        cond: Kind,
        body: Kind,
        else_: Kind,
    },

    Else {
        body: Kind,
    },

    Conditional {
        cond: Kind,
        then: Kind,
        else_: Kind,
    },

    DoWhile {
        body: Kind,
        cond: Kind,
    },

    For {
        init: Kind,
        cond: Kind,
        step: Kind,
        body: Kind,
    },

    Break,

    Call {
        name: String,
        args: Vec<Kind>,
    },

    Func {
        name: String,
        args: Vec<Kind>,
        body: Kind,
        stacksize: i32,
        ty: RefType,
        globals: Vec<Var>,
    },

    Statement {
        stmt: Kind,
        ty: RefType,
    },

    Expression {
        expr: Kind,
    },

    Compound {
        stmts: Vec<Kind>,
    },

    Comma {
        lhs: Kind,
        rhs: Kind,
    },

    Noop {},
}

impl Node {
    pub fn get_type(&self) -> Option<RefType> {
        match self {
            Node::Add { lhs, .. }
            | Node::Sub { lhs, .. }
            | Node::Mul { lhs, .. }
            | Node::Div { lhs, .. } => lhs.get_type(),

            Node::MulAssign { rhs, .. }
            | Node::DivAssign { rhs, .. }
            | Node::ModAssign { rhs, .. }
            | Node::AddAssign { rhs, .. }
            | Node::SubAssign { rhs, .. }
            | Node::AndAssign { rhs, .. }
            | Node::XorAssign { rhs, .. }
            | Node::OrAssign { rhs, .. }
            | Node::ShlAssign { rhs, .. }
            | Node::ShrAssign { rhs, .. }
            | Node::Assign { rhs, .. } => rhs.get_type(),

            Node::PostInc { expr }
            | Node::PostDec { expr }
            | Node::PreInc { expr }
            | Node::PreDec { expr } => expr.get_type(),

            Node::Expression { expr, .. }
            | Node::Neg { expr }
            | Node::Deref { expr }
            | Node::Dot { expr, .. } => expr.get_type(),

            Node::Addr { ty, .. }
            | Node::Constant { ty, .. }
            | Node::Statement { ty, .. }
            | Node::GVar { ty, .. }
            | Node::LVal { ty, .. }
            | Node::Vardef { ty, .. } => {
                let ty = Rc::clone(&ty);
                Some(ty)
            }

            Node::Func { body, .. } => body.get_type(),

            _ => None,
        }
    }

    pub(crate) fn set_type(&mut self, newtype: RefType) {
        use self::Node::*;

        match self {
            Or { lhs, rhs }
            | Xor { lhs, rhs }
            | And { lhs, rhs }
            | Mod { lhs, rhs }
            | Shr { lhs, rhs }
            | Shl { lhs, rhs }
            | Add { lhs, rhs }
            | Sub { lhs, rhs }
            | Mul { lhs, rhs }
            | Div { lhs, rhs } => {
                lhs.set_type(Rc::clone(&newtype));
                rhs.set_type(Rc::clone(&newtype));
            }

            Deref { expr, .. }
            | Dot { expr, .. }
            | Not { expr, .. }
            | Neg { expr }
            | PreInc { expr }
            | PreDec { expr }
            | PostInc { expr }
            | PostDec { expr } => expr.set_type(newtype),

            Node::MulAssign { lhs, .. }
            | Node::DivAssign { lhs, .. }
            | Node::ModAssign { lhs, .. }
            | Node::AddAssign { lhs, .. }
            | Node::SubAssign { lhs, .. }
            | Node::AndAssign { lhs, .. }
            | Node::XorAssign { lhs, .. }
            | Node::OrAssign { lhs, .. }
            | Node::ShlAssign { lhs, .. }
            | Node::ShrAssign { lhs, .. }
            | Assign { lhs, .. } => lhs.set_type(newtype),

            // TODO: this should only do the RHS
            Comma { lhs, rhs } => {
                lhs.set_type(Rc::clone(&newtype));
                rhs.set_type(Rc::clone(&newtype))
            }

            Conditional { then, .. } => then.set_type(newtype),

            Constant { ty, .. }
            | Str { ty, .. }
            | Addr { ty, .. }
            | LVal { ty, .. }
            | GVar { ty, .. }
            | Vardef { ty, .. }
            | Statement { ty, .. }
            | Func { ty, .. } => {
                ::std::mem::replace(ty, newtype);
            }
            _ => panic!("cannot set type: {} for {}", &*newtype.borrow(), self),
        };
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Comp {
    GreaterThan,   // > same as lt assembly-wise
    LessThan,      // <
    GreaterThanEq, // >=
    LessThanEq,    // <=
    Equal,         // ==
    NotEqual,      // !=
}

impl fmt::Display for Comp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let w = match self {
            Comp::GreaterThan => ">",
            Comp::LessThan => "<",
            Comp::GreaterThanEq => ">=",
            Comp::LessThanEq => "<=",
            Comp::Equal => "==",
            Comp::NotEqual => "!=",
        };
        write!(f, "{}", w)
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // this is poor but it saves a lot of editing
        let name = format!("{:?}", self);
        if let Some(pos) = name.find(' ') {
            return write!(f, "{}", &name[0..pos]);
        }

        write!(f, "{}", &name)
    }
}
