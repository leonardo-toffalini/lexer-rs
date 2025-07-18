#[derive(Debug)]
pub enum TokenType {
    Illegal,
    EOF,

    Ident,
    Int,

    // operators
    Assign, // =
    Plus,   // +
    Minus,  // -
    Bang,   // !
    Star,   // *
    Slash,  // /

    Eq,  // ==
    Neq, // !=
    Lt,  // <
    Le,  // <=
    Gt,  // >
    Ge,  // >=

    // delimiters,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,

    // keywords,
    Function,
    Let,
    TRUE,
    FALSE,
    If,
    Else,
    Return,
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
