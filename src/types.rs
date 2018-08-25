use super::*;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Char,
    Int,
    Ptr {
        ptr: Box<Type>,
    },
    Array {
        base: Box<Type>,
        len: usize,
        data: Vec<Type>,
    },
}

impl Type {
    pub fn is_ptr(&self) -> bool {
        match self {
            Type::Ptr { .. } => true,
            _ => false,
        }
    }

    pub fn ptr(&self) -> &Self {
        match self {
            Type::Ptr { ptr } => &ptr,
            _ => panic!("not a pointer"),
        }
    }

    pub fn ptr_mut(&mut self) -> &mut Self {
        match self {
            Type::Ptr { ref mut ptr } => ptr,
            _ => panic!("not a pointer"),
        }
    }

    pub fn ptr_of(&self) -> Self {
        Type::Ptr {
            ptr: Box::new(self.clone()),
        }
    }

    pub fn addr_of(&self, node: &Node) -> Node {
        Node::Addr {
            ty: self.ptr_of(),
            expr: Kind::make(node.clone()),
        }
    }

    pub fn size_of(&self) -> i32 {
        match self {
            Type::Char => 1,
            Type::Int => 4,
            Type::Ptr { .. } => 8,
            Type::Array { base, len, .. } => ((base.size_of() as usize) * len) as i32,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Char => write!(f, "Char"),
            Type::Int => write!(f, "Int"),
            Type::Ptr { ptr } => write!(f, "Ptr: {}", ptr),
            Type::Array { base, len, .. } => write!(f, "Arr of {}, {}", base, len),
        }
    }
}
