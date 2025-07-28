#[derive(Debug)]
pub enum Object {
    Integer { value: i64 },
    Boolean { value: bool },
    Null,
}
