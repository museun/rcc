use kind::Kind;
use semantics::Var;
use types::Type;

use std::fmt;

#[allow(unknown_lints, large_enum_variant)]
#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Constant {
        val: u32, // XXX: why is this a u32
        ty: Type,
    },

    Ident {
        name: String,
    },

    Str {
        str: String,
        ty: Type, // array
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

    LogAnd {
        lhs: Kind,
        rhs: Kind,
    },

    LogOr {
        lhs: Kind,
        rhs: Kind,
    },

    Equals {
        lhs: Kind,
        rhs: Kind,
    },

    NEquals {
        lhs: Kind,
        rhs: Kind,
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

    LVal {
        offset: i32,
        ty: Type,
    },

    GVar {
        name: String,
        ty: Type,
    },

    Addr {
        expr: Kind,
        ty: Type,
    },

    Deref {
        expr: Kind,
    },

    Vardef {
        ty: Type,
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

    Call {
        name: String,
        args: Vec<Kind>,
    },

    Func {
        name: String,
        args: Vec<Kind>,
        body: Kind,
        stacksize: i32,
        ty: Type,
        globals: Vec<Var>,
    },

    Statement {
        stmt: Kind,
        ty: Type,
    },

    Expression {
        expr: Kind,
    },

    Compound {
        stmts: Vec<Kind>,
    },

    Noop {},
}

impl Node {
    /// instrinsic types
    pub fn has_type(&self) -> bool {
        match self {
            Node::Constant { .. } | Node::GVar { .. } | Node::LVal { .. } | Node::Vardef { .. } => {
                true
            }
            _ => false,
        }
    }

    pub fn get_type(&self) -> &Type {
        match self {
            Node::Add { lhs, .. }
            | Node::Sub { lhs, .. }
            | Node::Mul { lhs, .. }
            | Node::Div { lhs, .. } => lhs.get_type(),

            Node::Addr { ty, .. } => ty,
            Node::Deref { expr } | Node::Dot { expr, .. } => expr.get_type(),

            Node::Constant { ty, .. }
            | Node::Statement { ty, .. }
            | Node::GVar { ty, .. }
            | Node::LVal { ty, .. }
            | Node::Vardef { ty, .. } => ty,
            _ => fail!("doesn't have a type\n{:#?}", self),
        }
    }

    pub(crate) fn set_type(&mut self, newtype: Type) {
        match self {
            Node::Add { lhs, rhs }
            | Node::Sub { lhs, rhs }
            | Node::Mul { lhs, rhs }
            | Node::Div { lhs, rhs } => {
                lhs.set_type(newtype.clone());
                rhs.set_type(newtype);
            }
            Node::Deref { expr, .. } | Node::Dot { expr, .. } => expr.set_type(newtype),
            Node::Assign { lhs, .. } => lhs.set_type(newtype),
            _ => unreachable!(),
        };
    }
}

impl AsMut<Node> for Node {
    fn as_mut(&mut self) -> &mut Node {
        self
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Comp {
    Gt, // same as lt assembly-wise
    Lt,
    Eq,
    NEq,
}

impl fmt::Display for Comp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let w = match self {
            Comp::Gt => ">",
            Comp::Lt => "<",
            Comp::Eq => "==",
            Comp::NEq => "!=",
        };
        write!(f, "{}", w)
    }
}
