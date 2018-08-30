use super::*;
use kind::Kind;
use semantics::Var;

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
        ty: RefType,
    },

    GVar {
        name: String,
        ty: RefType,
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

    pub fn get_type(&self) -> Option<RefType> {
        match self {
            Node::Add { lhs, .. }
            | Node::Sub { lhs, .. }
            | Node::Mul { lhs, .. }
            | Node::Div { lhs, .. } => lhs.get_type(),

            Node::Assign { rhs, .. } => rhs.get_type(),

            Node::Expression { expr, .. } | Node::Deref { expr } | Node::Dot { expr, .. } => {
                expr.get_type()
            }

            Node::Addr { ty, .. }
            | Node::Constant { ty, .. }
            | Node::Statement { ty, .. }
            | Node::GVar { ty, .. }
            | Node::LVal { ty, .. }
            | Node::Vardef { ty, .. } => Some(Rc::clone(&ty)),

            Node::Func { body, .. } => body.get_type(),

            _ => None,
        }
    }

    pub(crate) fn set_type(&mut self, newtype: RefType) {
        use self::Node::*;

        match self {
            Add { lhs, rhs } | Sub { lhs, rhs } | Mul { lhs, rhs } | Div { lhs, rhs } => {
                lhs.set_type(Rc::clone(&newtype));
                rhs.set_type(Rc::clone(&newtype));
            }
            Deref { expr, .. } | Dot { expr, .. } => expr.set_type(newtype),
            Assign { lhs, .. } => lhs.set_type(newtype),

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

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Node::*;

        match self {
            Constant { .. } => write!(f, "Constant"),
            Ident { .. } => write!(f, "Ident"),
            Str { .. } => write!(f, "Str"),
            Add { .. } => write!(f, "Add"),
            Sub { .. } => write!(f, "Sub"),
            Mul { .. } => write!(f, "Mul"),
            Div { .. } => write!(f, "Div"),
            LogAnd { .. } => write!(f, "LogAnd"),
            LogOr { .. } => write!(f, "LogOr"),
            Equals { .. } => write!(f, "Equals"),
            NEquals { .. } => write!(f, "NEquals"),
            Comparison { .. } => write!(f, "Comparison"),
            Assign { .. } => write!(f, "Assign"),
            LVal { .. } => write!(f, "LVal"),
            GVar { .. } => write!(f, "GVar"),
            Addr { .. } => write!(f, "Addr"),
            Deref { .. } => write!(f, "Deref"),
            Vardef { .. } => write!(f, "Vardef"),
            Struct { .. } => write!(f, "Struct"),
            Dot { .. } => write!(f, "Dot"),
            Return { .. } => write!(f, "Return"),
            Sizeof { .. } => write!(f, "Sizeof"),
            Alignof { .. } => write!(f, "Alignof"),
            If { .. } => write!(f, "If"),
            Else { .. } => write!(f, "Else"),
            DoWhile { .. } => write!(f, "DoWhile"),
            For { .. } => write!(f, "For"),
            Call { .. } => write!(f, "Call"),
            Func { .. } => write!(f, "Func"),
            Statement { .. } => write!(f, "Statement"),
            Expression { .. } => write!(f, "Expression"),
            Compound { .. } => write!(f, "Compound"),
            _ => Ok(()),
        }
    }
}
