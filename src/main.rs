use std::{fs::File, io, io::Read};

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod token;

fn read_file_contents(path: &str) -> Result<String, io::Error> {
    let mut file = File::open(path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    return Ok(contents);
}

fn main() -> Result<(), String> {
    let contents = read_file_contents("examples/let_statement.mk").unwrap();
    println!("File contents:\n{}", contents);

    let tokens = lexer::lex(&contents).unwrap();
    // println!("{:#?}", tokens);

    let ast = parser::parse(tokens);
    println!("{:#?}", ast);

    Ok(())
}

// Tests
#[cfg(test)]
mod tests {
    use crate::lexer;
    use crate::token::{Token, TokenType};

    #[test]
    fn lex_int_lit_test() {
        let source = "12345";
        let result = lexer::lex(source);
        let expected = vec![
            Token {
                ttype: TokenType::Int,
                literal: String::from("12345"),
            },
            Token {
                ttype: TokenType::EOF,
                literal: String::new(),
            },
        ];

        assert_eq!(result, Ok(expected));
    }

    #[test]
    fn lex_float_lit_test() {
        let source = "3.141592";
        let result = lexer::lex(source);
        let expected = vec![
            Token {
                ttype: TokenType::Float,
                literal: String::from("3.141592"),
            },
            Token {
                ttype: TokenType::EOF,
                literal: String::new(),
            },
        ];

        assert_eq!(result, Ok(expected));
    }

    #[test]
    fn lex_operators_test() {
        let source = "= + - ! * / == != < <= > >= , ; ( ) { }";
        let result = lexer::lex(source);
        let expected = vec![
            Token {
                ttype: TokenType::Assign,
                literal: String::from("="),
            },
            Token {
                ttype: TokenType::Plus,
                literal: String::from("+"),
            },
            Token {
                ttype: TokenType::Minus,
                literal: String::from("-"),
            },
            Token {
                ttype: TokenType::Bang,
                literal: String::from("!"),
            },
            Token {
                ttype: TokenType::Star,
                literal: String::from("*"),
            },
            Token {
                ttype: TokenType::Slash,
                literal: String::from("/"),
            },
            Token {
                ttype: TokenType::Eq,
                literal: String::from("=="),
            },
            Token {
                ttype: TokenType::Neq,
                literal: String::from("!="),
            },
            Token {
                ttype: TokenType::Lt,
                literal: String::from("<"),
            },
            Token {
                ttype: TokenType::Le,
                literal: String::from("<="),
            },
            Token {
                ttype: TokenType::Gt,
                literal: String::from(">"),
            },
            Token {
                ttype: TokenType::Ge,
                literal: String::from(">="),
            },
            Token {
                ttype: TokenType::Comma,
                literal: String::from(","),
            },
            Token {
                ttype: TokenType::Semicolon,
                literal: String::from(";"),
            },
            Token {
                ttype: TokenType::LParen,
                literal: String::from("("),
            },
            Token {
                ttype: TokenType::RParen,
                literal: String::from(")"),
            },
            Token {
                ttype: TokenType::LBrace,
                literal: String::from("{"),
            },
            Token {
                ttype: TokenType::RBrace,
                literal: String::from("}"),
            },
            Token {
                ttype: TokenType::EOF,
                literal: String::new(),
            },
        ];

        assert_eq!(result, Ok(expected));
    }

    #[test]
    fn lex_function_test() {
        let source = "
// this function adds two numbers
let adder = fn(a, b) {
	return a + b; // inline comment
}
// this is a comment without newline at the end";

        let result = lexer::lex(source);
        let expected = vec![
            Token {
                ttype: TokenType::Let,
                literal: String::from("let"),
            },
            Token {
                ttype: TokenType::Ident,
                literal: String::from("adder"),
            },
            Token {
                ttype: TokenType::Assign,
                literal: String::from("="),
            },
            Token {
                ttype: TokenType::Function,
                literal: String::from("fn"),
            },
            Token {
                ttype: TokenType::LParen,
                literal: String::from("("),
            },
            Token {
                ttype: TokenType::Ident,
                literal: String::from("a"),
            },
            Token {
                ttype: TokenType::Comma,
                literal: String::from(","),
            },
            Token {
                ttype: TokenType::Ident,
                literal: String::from("b"),
            },
            Token {
                ttype: TokenType::RParen,
                literal: String::from(")"),
            },
            Token {
                ttype: TokenType::LBrace,
                literal: String::from("{"),
            },
            Token {
                ttype: TokenType::Return,
                literal: String::from("return"),
            },
            Token {
                ttype: TokenType::Ident,
                literal: String::from("a"),
            },
            Token {
                ttype: TokenType::Plus,
                literal: String::from("+"),
            },
            Token {
                ttype: TokenType::Ident,
                literal: String::from("b"),
            },
            Token {
                ttype: TokenType::Semicolon,
                literal: String::from(";"),
            },
            Token {
                ttype: TokenType::RBrace,
                literal: String::from("}"),
            },
            Token {
                ttype: TokenType::EOF,
                literal: String::new(),
            },
        ];

        assert_eq!(result, Ok(expected));
    }

    #[test]
    fn lex_ident_test() {
        let source = "let foo = 12345;";
        let result = lexer::lex(source);
        let expected = vec![
            Token {
                ttype: TokenType::Let,
                literal: String::from("let"),
            },
            Token {
                ttype: TokenType::Ident,
                literal: String::from("foo"),
            },
            Token {
                ttype: TokenType::Assign,
                literal: String::from("="),
            },
            Token {
                ttype: TokenType::Int,
                literal: String::from("12345"),
            },
            Token {
                ttype: TokenType::Semicolon,
                literal: String::from(";"),
            },
            Token {
                ttype: TokenType::EOF,
                literal: String::new(),
            },
        ];

        assert_eq!(result, Ok(expected));
    }

    #[test]
    fn lex_string_literal_test() {
        let source = "let foo = \"Hello!\";";
        let result = lexer::lex(source);
        let expected = vec![
            Token {
                ttype: TokenType::Let,
                literal: String::from("let"),
            },
            Token {
                ttype: TokenType::Ident,
                literal: String::from("foo"),
            },
            Token {
                ttype: TokenType::Assign,
                literal: String::from("="),
            },
            Token {
                ttype: TokenType::String,
                literal: String::from("Hello!"),
            },
            Token {
                ttype: TokenType::Semicolon,
                literal: String::from(";"),
            },
            Token {
                ttype: TokenType::EOF,
                literal: String::new(),
            },
        ];

        assert_eq!(result, Ok(expected));
    }

    #[test]
    fn lex_keyword_test() {
        let source = "fn let true false if else return";
        let result = lexer::lex(source);
        let expected = vec![
            Token {
                ttype: TokenType::Function,
                literal: String::from("fn"),
            },
            Token {
                ttype: TokenType::Let,
                literal: String::from("let"),
            },
            Token {
                ttype: TokenType::TRUE,
                literal: String::from("true"),
            },
            Token {
                ttype: TokenType::FALSE,
                literal: String::from("false"),
            },
            Token {
                ttype: TokenType::If,
                literal: String::from("if"),
            },
            Token {
                ttype: TokenType::Else,
                literal: String::from("else"),
            },
            Token {
                ttype: TokenType::Return,
                literal: String::from("return"),
            },
            Token {
                ttype: TokenType::EOF,
                literal: String::new(),
            },
        ];

        assert_eq!(result, Ok(expected));
    }
}
