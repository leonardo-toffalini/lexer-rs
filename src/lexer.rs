use crate::token::Token;
use crate::token::TokenType;

pub fn lex(source: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut chars = source.chars().peekable();

    loop {
        match chars.next() {
            Some('+') => tokens.push(Token {
                ttype: TokenType::Plus,
                literal: String::from("+"),
            }),
            Some('-') => tokens.push(Token {
                ttype: TokenType::Minus,
                literal: String::from("-"),
            }),
            Some(' ') => (),
            Some('\n') => (),
            Some('\t') => (),
            Some('\r') => (),
            Some('=') => match chars.peek() {
                Some('=') => tokens.push(Token {
                    ttype: TokenType::Eq,
                    literal: String::from("=="),
                }),
                Some(_) => {
                    chars.next();
                    tokens.push(Token {
                        ttype: TokenType::Assign,
                        literal: String::from("="),
                    });
                }
                None => {
                    chars.next();
                    tokens.push(Token {
                        ttype: TokenType::Assign,
                        literal: String::from("="),
                    });
                }
            },
            Some(c) => tokens.push(Token {
                ttype: TokenType::Int,
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
