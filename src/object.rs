use std::fmt;

use crate::ast::{Expression, Statement};
use crate::environment::Env;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer {
        value: i64,
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
            Object::Boolean { .. } => "Boolean",
            Object::ReturnValue { .. } => "ReturnValue",
            Object::Function { .. } => "Function",
            Object::Error { .. } => "Error",
            Object::Null => "Null",
        }
    }
}
