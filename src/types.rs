use super::*;
use kind::Kind;
use node::Node;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Char,
    Int,
    Void,
    Ptr {
        size: i32,
        align: i32,
        ptr: RefType,
    },
    Array {
        size: i32,
        align: i32,
        base: RefType,
        len: usize,
        data: Vec<RefType>,
    },
    Struct {
        size: i32,
        align: i32,
        members: Vec<Node>, // Vardef
    },
}

impl Type {
    pub fn add_members(&mut self, nodes: &[Node]) {
        if let Type::Struct {
            size,
            align,
            members,
        } = self
        {
            let mut os = 0;
            for node in nodes {
                if let Node::Vardef { ty, .. } = node {
                    let al = align_of(&*ty.borrow());
                    let sz = size_of(&*ty.borrow());

                    os = round(os, al);

                    let mut newnode = node.clone();
                    match &mut newnode {
                        Node::Vardef { offset, .. } => {
                            *offset = os;
                        }
                        _ => unreachable!(),
                    }
                    members.push(newnode);

                    os += sz;
                    if *align < al {
                        *align = al;
                    }
                }
            }

            *size = round(os, *align)
        } else {
            panic!("can only add members to structs")
        }
    }
}

pub fn as_ptr(ty: &RefType) -> Option<RefType> {
    match &*ty.borrow() {
        Type::Ptr { ptr, .. } => Some(Rc::clone(&ptr)),
        _ => None,
    }
}

pub fn ptr_of(ty: &RefType) -> Type {
    Type::Ptr {
        size: 8,
        align: 8,
        ptr: Rc::clone(&ty),
    }
}

pub fn addr_of(ty: &RefType, node: &Node) -> Node {
    Node::Addr {
        ty: Rc::new(RefCell::new(ptr_of(&ty))),
        expr: Kind::make(node.clone()),
    }
}

pub fn array_of(ty: &RefType, len: usize) -> Type {
    Type::Array {
        size: size_of(&*ty.borrow()),
        align: align_of(&*ty.borrow()),
        base: Rc::clone(&ty),
        len,
        data: vec![],
    }
}

pub fn size_of(ty: &Type) -> i32 {
    match ty {
        Type::Char => 1,
        Type::Int => 4,
        Type::Void => 0,
        Type::Ptr { size, .. } => *size,
        Type::Array { base, len, .. } => ((size_of(&*base.borrow()) as usize) * len) as i32,
        Type::Struct { size, .. } => *size,
    }
}

pub fn align_of(ty: &Type) -> i32 {
    match ty {
        Type::Char => 1,
        Type::Int => 4,
        Type::Void => 0,
        Type::Ptr { align, .. } => *align,
        Type::Array { base, .. } => align_of(&*base.borrow()),
        Type::Struct { align, .. } => *align,
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Var {
    pub ty: RefType,
    pub offset: i32,
    pub global: Option<(String, String)>, // None = local
    pub is_extern: bool,
}

impl Var {
    pub fn global(ty: RefType, name: &str, s: &str, is_extern: bool) -> Self {
        Self {
            ty,
            offset: 0,
            global: Some((name.into(), s.into())),
            is_extern,
        }
    }
}

use std::fmt;
impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Var {} (extern: {}, offset: {}) = {}",
            &*self.ty.borrow(),
            self.is_extern,
            self.offset,
            match &self.global {
                Some((k, v)) => format!("\"{}\" -> \"{}\"", k, v),
                None => "None".into(),
            }
        )
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Type::*;

        match self {
            Char => write!(f, "Char")?,
            Int => write!(f, "Int")?,
            Void => write!(f, "Void")?,
            Ptr { .. } => write!(f, "Ptr")?,
            Array { .. } => write!(f, "Array")?,
            Struct { .. } => write!(f, "Struct")?,
        };

        Ok(())
    }
}
