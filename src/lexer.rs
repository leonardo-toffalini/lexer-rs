use crate::token::Token;
use crate::token::TokenType;
use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::iter::Peekable;
use std::str::Chars;

// I have no clue what is going on here, I just wanted a global map of keywords.
static KW_MAP: Lazy<HashMap<&'static str, TokenType>> = Lazy::new(|| {
    let mut map = HashMap::new();
    map.insert("fn", TokenType::Function);
    map.insert("let", TokenType::Let);
    map.insert("true", TokenType::TRUE);
    map.insert("false", TokenType::FALSE);
    map.insert("if", TokenType::If);
    map.insert("else", TokenType::Else);
    map.insert("return", TokenType::Return);
    return map;
});

pub fn lex(source: &str) -> Result<Vec<Token>, String> {
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

            // delimiters
            Some(',') => tokens.push(Token {
                ttype: TokenType::Comma,
                literal: String::from(","),
            }),
            Some(';') => tokens.push(Token {
                ttype: TokenType::Semicolon,
                literal: String::from(";"),
            }),
            Some('(') => tokens.push(Token {
                ttype: TokenType::LParen,
                literal: String::from("("),
            }),
            Some(')') => tokens.push(Token {
                ttype: TokenType::RParen,
                literal: String::from(")"),
            }),
            Some('{') => tokens.push(Token {
                ttype: TokenType::LBrace,
                literal: String::from("{"),
            }),
            Some('}') => tokens.push(Token {
                ttype: TokenType::RBrace,
                literal: String::from("}"),
            }),

            // ident
            Some(c) if c.is_ascii_alphabetic() || c == '_' => {
                let ident = lex_ident(&mut chars, c);
                let ttype = lookup_keyword(&ident);
                tokens.push(Token {
                    ttype: ttype,
                    literal: ident,
                });
            }

            // int
            Some(c) if c.is_digit(10) => match lex_int_lit(&mut chars, c) {
                Numeric::Int(int_lit) => tokens.push(Token {
                    ttype: TokenType::Int,
                    literal: int_lit,
                }),
                Numeric::Float(float_lit) => tokens.push(Token {
                    ttype: TokenType::Float,
                    literal: float_lit,
                }),
            },

            // string
            Some('"') => {
                let string_literal = lex_str_lit(&mut chars)?;
                tokens.push(Token {
                    ttype: TokenType::String,
                    literal: string_literal,
                })
            }

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

    return Ok(tokens);
}

fn lex_ident(chars: &mut Peekable<Chars>, first_char: char) -> String {
    let mut ident = String::from(first_char);

    loop {
        match chars.peek() {
            Some(c) if c.is_ascii_alphanumeric() || *c == '_' => {
                ident.push(*c);
                chars.next();
            }
            Some(_) | None => break,
        }
    }

    return ident;
}

enum Numeric {
    Int(String),
    Float(String),
}

fn lex_int_lit(chars: &mut Peekable<Chars>, first_char: char) -> Numeric {
    let mut literal = String::from(first_char);
    let mut float_flag = false;

    loop {
        match chars.peek() {
            Some('.') => {
                chars.next();
                literal.push('.');
                float_flag = true;
            }
            Some(c) if c.is_digit(10) => {
                literal.push(*c);
                chars.next();
            }
            Some(_) | None => break,
        }
    }

    return if float_flag {
        Numeric::Float(literal)
    } else {
        Numeric::Int(literal)
    };
}

fn lex_str_lit(chars: &mut Peekable<Chars>) -> Result<String, String> {
    let mut int_lit = String::from("");

    loop {
        match chars.next() {
            Some('"') => return Ok(int_lit),
            Some(c) => int_lit.push(c),
            None => return Err(String::from("Lexical Error: Unterminated string literal.")),
        }
    }
}

fn lookup_keyword(ident: &str) -> TokenType {
    match KW_MAP.get(ident) {
        Some(kw_ttype) => *kw_ttype,
        None => TokenType::Ident,
    }
}

// pub fn look_up_ident(ident: String) -> TokenType {
//   let kw = [
//     #("fn", Function),
//     #("let", Let),
//     #("true", TRUE),
//     #("false", FALSE),
//     #("if", If),
//     #("else", Else),
//     #("return", Return),
//   ]
//   let keywords = dict.from_list(kw)
//   case dict.get(keywords, ident) {
//     Ok(v) -> v
//     Error(Nil) -> Ident
//   }
// }
