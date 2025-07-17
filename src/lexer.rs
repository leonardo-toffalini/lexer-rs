use crate::token::Token;
use crate::token::TokenType;

pub fn lex(mut source: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();

    while !source.is_empty() {
        let first = source.as_bytes()[0] as char;
        source = &source[1..];

        if first.is_digit(10) {
            tokens.push(Token {
                ttype: TokenType::Int,
                literal: first.to_string(),
            });
        }

        if first == '+' {
            tokens.push(Token {
                ttype: TokenType::Plus,
                literal: first.to_string(),
            });
        }
    }
    tokens.push(Token::eof());
    return tokens;
}
