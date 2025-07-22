use std::collections::HashMap;

use crate::ast;
use crate::token::{Token, TokenType};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
}

pub struct Parser {
    idx: usize,
    cur_token: Token,
    peek_token: Token,
    tokens: Vec<Token>,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

type PrefixParseFn = for<'a> fn(&'a mut Parser) -> ast::Expression;
type InfixParseFn = for<'a> fn(&'a mut Parser, ast::Expression) -> ast::Expression;

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        let cur_token = tokens.get(0).unwrap();
        let peek_token = tokens.get(1).unwrap();
        let mut parser = Parser {
            idx: 0,
            cur_token: cur_token.clone(),
            peek_token: peek_token.clone(),
            tokens,
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        parser.register_prefix(TokenType::Ident, Parser::parse_identifier);

        return parser;
    }

    fn next_token(self: &mut Self) {
        self.idx += 1;
        self.cur_token = self.peek_token.clone();
        self.peek_token = match self.tokens.get(self.idx + 1) {
            Some(t) => t.clone(),
            None => Token::eof(),
        };
    }

    fn register_prefix(self: &mut Self, ttype: TokenType, f: PrefixParseFn) {
        self.prefix_parse_fns.insert(ttype, f);
    }

    fn register_infix(self: &mut Self, ttype: TokenType, f: InfixParseFn) {
        self.infix_parse_fns.insert(ttype, f);
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
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
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

    fn parse_return_statement(self: &mut Self) -> ast::Statement {
        self.next_token();
        while !self.cur_token_is(TokenType::Semicolon) {
            self.next_token();
        }
        return ast::Statement::ReturnStatement {
            value: ast::Expression::EmptyExpression,
        };
    }

    fn parse_expression_statement(self: &mut Self) -> ast::Statement {
        let expr = self.parse_expression(Precedence::Lowest);

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        return ast::Statement::ExpressionStatement { expr };
    }

    fn parse_expression(self: &mut Self, prec: Precedence) -> ast::Expression {
        let prefix = match self.prefix_parse_fns.get(&self.cur_token.ttype) {
            Some(v) => v,
            None => panic!(
                "Did not find prefix parse fn with ttype: {:#?}",
                self.cur_token.ttype
            ),
        };

        let left_expr = prefix();
        return left_expr;
    }

    fn parse_identifier(self: &mut Self) -> ast::Expression {}

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
