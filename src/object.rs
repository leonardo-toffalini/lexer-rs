#[derive(Debug, PartialEq)]
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
