use core::panic;
use std::iter::Peekable;
use std::slice::Iter;

use crate::ast;
use crate::token::{Token, TokenType};

type ProgramResult = Result<ast::Program, String>;
type StatementResult = Result<ast::Statement, String>;
type TokIter<'a> = Iter<'a, Token>;

pub struct Parser {
    idx: usize,
    cur_token: Token,
    peek_token: Token,
    tokens: Vec<Token>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        let cur_token = tokens.get(0).unwrap();
        let peek_token = tokens.get(1).unwrap();
        Parser {
            idx: 0,
            cur_token: cur_token.clone(),
            peek_token: peek_token.clone(),
            tokens,
        }
    }

    fn next_token(self: &mut Self) {
        self.cur_token = self.peek_token.clone();
        match self.tokens.get(self.idx) {
            Some(t) => self.peek_token = t.clone(),
            None => panic!("Tokens terminated without EOF!"),
        }
        self.idx += 1;
    }

    pub fn parse(self: &mut Self) {
        let mut program = ast::Program::new();
        while self.cur_token.ttype != TokenType::EOF {
            let stmt = self.parse_statement();
            program.statements.push(stmt);
            self.next_token();
        }
    }

    fn parse_statement(self: &mut Self) -> ast::Statement {
        match self.cur_token.ttype {
            TokenType::Let => self.parse_let_statement(),
            _ => panic!("Can only parse let statements"),
        }
    }

    fn parse_let_statement(self: &mut Self) -> ast::Statement {
        println!("cur_token: {:#?}", self.cur_token);
        println!("peek_token: {:#?}", self.peek_token);
        if !self.expect_peek(TokenType::Ident) {
            panic!("Expected Ident");
        }

        println!("cur_token: {:#?}", self.cur_token);
        println!("peek_token: {:#?}", self.peek_token);

        let ident = ast::Identifier {
            value: self.cur_token.literal.clone(),
        };

        if !self.expect_peek(TokenType::Assign) {
            panic!("Expected Assign");
        }

        while !self.cur_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        return ast::Statement::LetStatement {
            name: ident,
            value: ast::Expression::EmptyExpression,
        };
    }

    fn cur_token_is(self: &mut Self, ttype: TokenType) -> bool {
        return ttype == self.cur_token.ttype;
    }

    fn peek_token_is(self: &mut Self, ttype: TokenType) -> bool {
        return ttype == self.peek_token.ttype;
    }

    fn expect_peek(self: &mut Self, ttype: TokenType) -> bool {
        if self.peek_token_is(ttype) {
            self.next_token();
            return true;
        } else {
            return false;
        }
    }
}

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
            ..
        } => return parse_let_statement(tokens_iter),
        Token {
            ttype: TokenType::Return,
            ..
        } => return parse_return_statement(tokens_iter),
        _ => return Err(String::from("Unexpected statement.")),
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

fn parse_return_statement(tokens_iter: &mut Peekable<TokIter>) -> StatementResult {
    loop {
        match tokens_iter.next() {
            Some(Token {
                ttype: TokenType::Semicolon,
                ..
            }) => break,
            None => return Err(String::from("Expected semicolon")),
            _ => (), // Skip/consume or handle other tokens
        }
    }
    return Ok(ast::Statement::ReturnStatement {
        value: ast::Expression::EmptyExpression,
    });
}
