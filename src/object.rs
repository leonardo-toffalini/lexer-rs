use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Integer { value: i64 },
    Boolean { value: bool },
    ReturnValue { value: Box<Object> },
    Null,
}

impl Object {
    pub fn new_false() -> Object {
        Object::Boolean { value: false }
    }
    pub fn new_true() -> Object {
        Object::Boolean { value: true }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer { value } => write!(f, "{value}"),
            Object::Boolean { value } => write!(f, "{value}"),
            Object::ReturnValue { value } => write!(f, "{value}"),
            Object::Null => write!(f, ""),
        }
    }
}
