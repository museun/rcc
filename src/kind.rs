use super::*;
use node::Node;

#[derive(Debug, Clone, PartialEq)]
pub struct Kind {
    pub(crate) val: Option<Box<Node>>, // node uses kind
    pub(crate) ty: Option<RefType>,
}

impl Kind {
    pub fn make(node: Node) -> Self {
        Kind {
            val: Some(Box::new(node)),
            ty: None,
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

    pub fn get_val(&self) -> &Node {
        self.val.as_ref().unwrap()
    }

    pub fn get_val_mut(&mut self) -> &mut Node {
        self.val.as_mut().unwrap()
    }

    pub fn get_type(&self) -> Option<RefType> {
        match self.ty.as_ref() {
            Some(ty) => Some(Rc::clone(&ty)),
            None => self.get_val().get_type(),
        }
    }

    pub fn set_type(&mut self, newtype: RefType) {
        match &mut self.ty {
            Some(ty) => {
                ::std::mem::replace(ty, newtype);
            }
            None => {
                self.ty = Some(newtype);
            }
        };
    }
}

impl AsRef<Node> for Kind {
    fn as_ref(&self) -> &Node {
        self.val.as_ref().unwrap()
    }
}

impl AsMut<Node> for Kind {
    fn as_mut(&mut self) -> &mut Node {
        self.val.as_mut().unwrap()
    }
}

use std::fmt;
impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.val.is_some() {
            let v = self.val.as_ref().unwrap();
            write!(f, "{}", v)?;
        } else {
            write!(f, "None")?;
        }

        if self.ty.is_some() {
            let ty = self.ty.as_ref().unwrap();
            write!(f, ": {}", &*ty.borrow())?;
        } else {
            write!(f, ": None")?;
        }

        Ok(())
    }
}
