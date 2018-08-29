use kind::Kind;
use node::Node;
use util::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Char,
    Int,
    Ptr {
        size: i32,
        align: i32,
        ptr: Box<Type>,
    },
    Array {
        size: i32,
        align: i32,
        base: Box<Type>,
        len: usize,
        data: Vec<Type>,
    },
    Struct {
        size: i32,
        align: i32,
        members: Vec<Node>, // Vardef
    },
}

impl Type {
    pub fn as_ptr(&self) -> Option<&Self> {
        match self {
            Type::Ptr { ref ptr, .. } => Some(ptr),
            _ => None,
        }
    }

    pub fn as_ptr_mut(&mut self) -> Option<&mut Self> {
        match self {
            Type::Ptr { ref mut ptr, .. } => Some(ptr),
            _ => None,
        }
    }

    pub fn ptr_of(&self) -> Self {
        Type::Ptr {
            size: 8,
            align: 8,
            ptr: Box::new(self.clone()),
        }
    }

    pub fn array_of(&self, len: usize) -> Self {
        Type::Array {
            size: self.size(),
            align: self.align(),
            base: Box::new(self.clone()),
            len,
            data: vec![],
        }
    }

    pub fn struct_of(nodes: &[Node]) -> Self {
        let mut members = nodes
            .iter()
            .filter_map(|n| match n {
                Node::Vardef { .. } => Some(n.clone()),
                _ => None,
            }).collect::<Vec<_>>();
        let mut os = 0;
        let mut align = 0;

        for node in &mut members {
            if let Node::Vardef {
                ty, ref mut offset, ..
            } = node
            {
                os = round(os, ty.align());
                *offset = os;
                os += ty.size();
                if align < ty.align() {
                    align = ty.align();
                }
            } else {
                unreachable!()
            }
        }

        Type::Struct {
            members,
            size: round(os, align),
            align,
        }
    }

    // TODO this doesn't belong here
    pub fn addr_of(&self, node: &Node) -> Node {
        Node::Addr {
            ty: self.ptr_of(),
            expr: Kind::make(node.clone()),
        }
    }
}

pub trait SizeOf {
    fn size(&self) -> i32;
}

pub trait AlignOf {
    fn align(&self) -> i32;
}

impl SizeOf for Type {
    fn size(&self) -> i32 {
        match self {
            Type::Char => 1,
            Type::Int => 4,
            &Type::Ptr { size, .. } => size,
            Type::Array { base, len, .. } => ((base.size() as usize) * len) as i32,
            &Type::Struct { size, .. } => size,
        }
    }
}

impl AlignOf for Type {
    fn align(&self) -> i32 {
        match self {
            Type::Char => 1,
            Type::Int => 4,
            &Type::Ptr { align, .. } => align,
            Type::Array { base, .. } => base.align(),
            &Type::Struct { align, .. } => align,
        }
    }
}

// TODO redo this
// impl fmt::Display for Type {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         match self {
//             Type::Char => write!(f, "Char"),
//             Type::Int => write!(f, "Int"),
//             Type::Ptr { ptr, size, align } => write!(f, "Ptr: {} ({},{})",
// ptr, size, align),             Type::Array { base, len, .. } => write!(f,
// "Arr of {}, {}", base, len),             Type::Struct { members, .. } => {
//                 write!(f, "Struct {{")?;
//                 for (i, member) in members.iter().enumerate() {
//                     write!(f, "{:?}", member)?;
//                     if i < members.len() {
//                         write!(f, ", ")?;
//                     }
//                 }
//                 write!(f, "}}")
//             }
//         }
//     }
// }
