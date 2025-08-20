use std::fmt;

use crate::ast::{Expression, Statement};
use crate::environment::Env;

#[derive(Debug, Clone)]
pub enum Object {
    Integer {
        value: i64,
    },
    Float {
        value: f64,
    },
    String {
        value: String,
    },
    Boolean {
        value: bool,
    },
    ReturnValue {
        value: Box<Object>,
    },
    Function {
        parameters: Vec<Expression>,
        body: Statement,
        env: Env,
    },
    Error {
        message: String,
    },
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
            Object::Float { value } => write!(f, "{value}"),
            Object::String { value } => write!(f, "{value}"),
            Object::Boolean { value } => write!(f, "{value}"),
            Object::ReturnValue { value } => write!(f, "{value}"),
            Object::Function { .. } => write!(f, "function"), // todo
            Object::Error { message } => write!(f, "Error: {message}"),
            Object::Null => write!(f, ""),
        }
    }
}

impl Object {
    pub fn mytype(self: &Self) -> &str {
        match self {
            Object::Integer { .. } => "Integer",
            Object::Float { .. } => "Float",
            Object::String { .. } => "String",
            Object::Boolean { .. } => "Boolean",
            Object::ReturnValue { .. } => "ReturnValue",
            Object::Function { .. } => "Function",
            Object::Error { .. } => "Error",
            Object::Null => "Null",
        }
    }
}

// copied generated derived PartialEq trait to overwrite float comparison
// this code needs to be changed if a new Object variant is added!
impl ::core::cmp::PartialEq for Object {
    #[inline]
    fn eq(&self, other: &Object) -> bool {
        let __self_discr = std::mem::discriminant(self);
        let __arg1_discr = std::mem::discriminant(other);
        __self_discr == __arg1_discr
            && match (self, other) {
                (Object::Integer { value: __self_0 }, Object::Integer { value: __arg1_0 }) => {
                    __self_0 == __arg1_0
                }
                (Object::Float { value: __self_0 }, Object::Float { value: __arg1_0 }) => {
                    __self_0 - __arg1_0 < 1e-8 // this is the only changed part
                }
                (Object::String { value: __self_0 }, Object::String { value: __arg1_0 }) => {
                    __self_0 == __arg1_0
                }
                (Object::Boolean { value: __self_0 }, Object::Boolean { value: __arg1_0 }) => {
                    __self_0 == __arg1_0
                }
                (
                    Object::ReturnValue { value: __self_0 },
                    Object::ReturnValue { value: __arg1_0 },
                ) => __self_0 == __arg1_0,
                (
                    Object::Function {
                        parameters: __self_0,
                        body: __self_1,
                        env: __self_2,
                    },
                    Object::Function {
                        parameters: __arg1_0,
                        body: __arg1_1,
                        env: __arg1_2,
                    },
                ) => __self_0 == __arg1_0 && __self_1 == __arg1_1 && __self_2 == __arg1_2,
                (Object::Error { message: __self_0 }, Object::Error { message: __arg1_0 }) => {
                    __self_0 == __arg1_0
                }
                _ => true,
            }
    }
}
