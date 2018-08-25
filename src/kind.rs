use super::*;

#[derive(Debug, Clone, PartialEq)]
pub struct Kind {
    pub(crate) val: Option<Box<Node>>, // node uses kind
    pub(crate) ty: Option<Type>,
}

impl Kind {
    pub fn make(node: Node) -> Self {
        let ty = if node.has_type() {
            Some(node.get_type().clone()) // sad
        } else {
            None
        };

        Kind {
            val: Some(Box::new(node)),
            ty,
        }
    }

    pub fn empty() -> Self {
        Kind {
            val: None,
            ty: None,
        }
    }

    pub fn has_val(&self) -> bool {
        self.val.is_some()
    }

    pub fn has_type(&self) -> bool {
        match self.ty.as_ref() {
            None => self.get_val().has_type(),
            Some(_) => true,
        }
    }

    pub fn get_val(&self) -> &Node {
        self.val.as_ref().unwrap()
    }

    pub fn get_val_mut(&mut self) -> &mut Node {
        self.val.as_mut().unwrap()
    }

    pub fn get_type(&self) -> &Type {
        match self.ty.as_ref() {
            None => self.get_val().get_type(),
            Some(ty) => ty,
        }
    }

    pub fn set_type(&mut self, ty: Type) {
        let _ = self.ty.get_or_insert(ty);
    }
}
