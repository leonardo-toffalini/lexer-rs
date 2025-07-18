#[derive(Debug)]
pub enum TokenType {
    Illegal,
    EOF,

    Int,
    Plus,
    Minus,
    Eq,
    Assign,
}

#[derive(Debug)]
pub struct Token {
    pub ttype: TokenType,
    pub literal: String,
}

impl Token {
    pub fn eof() -> Self {
        Self {
            ttype: TokenType::EOF,
            literal: String::from(""),
        }
    }
}
