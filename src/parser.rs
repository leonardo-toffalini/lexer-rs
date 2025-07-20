use std::iter::Peekable;
use std::slice::Iter;

use crate::ast;
use crate::token::{Token, TokenType};

type ProgramResult = Result<ast::Program, String>;
type StatementResult = Result<ast::Statement, String>;
type TokIter<'a> = Iter<'a, Token>;

pub fn parse(tokens: Vec<Token>) -> ProgramResult {
    let mut program = ast::Program::new();
    let mut tokens_iter = tokens.iter().peekable();

    loop {
        match tokens_iter.next() {
            Some(Token {
                ttype: TokenType::EOF,
                literal: _,
            })
            | None => break,

            Some(tok) => {
                let stmt = parse_statement(&mut tokens_iter, tok)?;
                program.push(stmt);
            }
        }
    }

    return Ok(program);
}

fn parse_statement(tokens_iter: &mut Peekable<TokIter>, tok: &Token) -> StatementResult {
    match tok {
        Token {
            ttype: TokenType::Let,
            literal: _,
        } => return parse_let_statement(tokens_iter),
        _ => return Err(String::from("Unexpected statment.")),
    }
}

fn parse_let_statement(tokens_iter: &mut Peekable<TokIter>) -> StatementResult {
    let name = match tokens_iter.peek() {
        Some(Token {
            ttype: TokenType::Ident,
            literal,
        }) => literal,
        _ => return Err(String::from("Expected Ident")),
    };
    tokens_iter.next();

    match tokens_iter.peek() {
        Some(Token {
            ttype: TokenType::Assign,
            ..
        }) => {}
        _ => return Err(String::from("Expected Assign")),
    }
    tokens_iter.next();

    loop {
        match tokens_iter.next() {
            Some(Token {
                ttype: TokenType::Semicolon,
                ..
            }) => {
                return Ok(ast::Statement::LetStatement {
                    name: ast::Identifier {
                        value: name.to_string(),
                    },
                    value: ast::Expression::EmptyExpression,
                });
            }
            None => return Err(String::from("Expected semicolon")),
            _ => (), // Skip/consume or handle other tokens
        }
    }
}
