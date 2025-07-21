use crate::ast;
use crate::token::{Token, TokenType};

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
        self.idx += 1;
        self.cur_token = self.peek_token.clone();
        self.peek_token = match self.tokens.get(self.idx + 1) {
            Some(t) => t.clone(),
            None => Token::eof(),
        };
    }

    pub fn parse(self: &mut Self) -> ast::Program {
        let mut program = ast::Program::new();
        while self.cur_token.ttype != TokenType::EOF {
            let stmt = self.parse_statement();
            program.statements.push(stmt);
            self.next_token();
        }
        return program;
    }

    fn parse_statement(self: &mut Self) -> ast::Statement {
        match self.cur_token.ttype {
            TokenType::Let => self.parse_let_statement(),
            _ => panic!("Can only parse let statements"),
        }
    }

    fn parse_let_statement(self: &mut Self) -> ast::Statement {
        if !self.expect_peek(TokenType::Ident) {
            panic!("Expected Ident");
        }

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
