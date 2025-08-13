#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Object {
    Integer { value: i64 },
    Boolean { value: bool },
    Null,
}
