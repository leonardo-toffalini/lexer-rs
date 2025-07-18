use crate::token::Token;
use crate::token::TokenType;

pub fn lex(source: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut chars = source.chars().peekable();

    loop {
        match chars.next() {
            // whitespace
            Some(' ') => (),
            Some('\n') => (),
            Some('\t') => (),
            Some('\r') => (),

            // operators
            Some('+') => tokens.push(Token {
                ttype: TokenType::Plus,
                literal: String::from("+"),
            }),
            Some('-') => tokens.push(Token {
                ttype: TokenType::Minus,
                literal: String::from("-"),
            }),
            Some('*') => tokens.push(Token {
                ttype: TokenType::Star,
                literal: String::from("*"),
            }),
            Some('/') => tokens.push(Token {
                ttype: TokenType::Slash,
                literal: String::from("/"),
            }),
            // = or ==
            Some('=') => match chars.peek() {
                Some('=') => {
                    chars.next();
                    tokens.push(Token {
                        ttype: TokenType::Eq,
                        literal: String::from("=="),
                    })
                }
                Some(_) | None => tokens.push(Token {
                    ttype: TokenType::Assign,
                    literal: String::from("="),
                }),
            },
            // ! or !=
            Some('!') => match chars.peek() {
                Some('=') => {
                    chars.next();
                    tokens.push(Token {
                        ttype: TokenType::Neq,
                        literal: String::from("!="),
                    })
                }
                Some(_) | None => tokens.push(Token {
                    ttype: TokenType::Bang,
                    literal: String::from("!"),
                }),
            },
            // < or <=
            Some('<') => match chars.peek() {
                Some('=') => {
                    chars.next();
                    tokens.push(Token {
                        ttype: TokenType::Le,
                        literal: String::from("<="),
                    })
                }
                Some(_) | None => tokens.push(Token {
                    ttype: TokenType::Lt,
                    literal: String::from("<"),
                }),
            },
            // > or >=
            Some('>') => match chars.peek() {
                Some('=') => {
                    chars.next();
                    tokens.push(Token {
                        ttype: TokenType::Ge,
                        literal: String::from(">="),
                    })
                }
                Some(_) | None => tokens.push(Token {
                    ttype: TokenType::Gt,
                    literal: String::from(">"),
                }),
            },

            // other
            Some(c) => tokens.push(Token {
                ttype: TokenType::Illegal,
                literal: c.to_string(),
            }),
            None => {
                tokens.push(Token::eof());
                break;
            }
        }
    }

    return tokens;
}
