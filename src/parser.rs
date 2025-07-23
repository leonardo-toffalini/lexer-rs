use std::collections::HashMap;

use crate::ast;
use crate::token::{Token, TokenType};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equality,       // == or !=
    LessGreater,    // > or <
    Additive,       // + or -
    Multiplicative, // * or /
    Prefix,         // -X or !X
    Call,           // myFunction(X)
}

pub struct Parser {
    idx: usize,
    cur_token: Token,
    peek_token: Token,
    tokens: Vec<Token>,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
    precedences: HashMap<TokenType, Precedence>,
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
            precedences: HashMap::new(),
        };

        parser
            .precedences
            .insert(TokenType::Eq, Precedence::Equality);
        parser
            .precedences
            .insert(TokenType::Neq, Precedence::Equality);
        parser
            .precedences
            .insert(TokenType::Lt, Precedence::LessGreater);
        parser
            .precedences
            .insert(TokenType::Gt, Precedence::LessGreater);
        parser
            .precedences
            .insert(TokenType::Plus, Precedence::Additive);
        parser
            .precedences
            .insert(TokenType::Minus, Precedence::Additive);
        parser
            .precedences
            .insert(TokenType::Star, Precedence::Multiplicative);
        parser
            .precedences
            .insert(TokenType::Slash, Precedence::Multiplicative);

        parser.register_prefix(TokenType::Ident, Parser::parse_identifier);
        parser.register_prefix(TokenType::Int, Parser::parse_integer_literal);
        parser.register_prefix(TokenType::Bang, Parser::parse_prefix_expression);
        parser.register_prefix(TokenType::Minus, Parser::parse_prefix_expression);
        parser.register_prefix(TokenType::TRUE, Parser::parse_boolean);
        parser.register_prefix(TokenType::FALSE, Parser::parse_boolean);
        parser.register_prefix(TokenType::LParen, Parser::parse_grouped_expression);

        parser.register_infix(TokenType::Plus, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Minus, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Star, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Slash, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Eq, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Neq, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Lt, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Gt, Parser::parse_infix_expression);

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

        let ident = ast::Expression::Identifier {
            name: self.cur_token.literal.clone(),
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

    fn parse_expression(&mut self, prec: Precedence) -> ast::Expression {
        let prefix = self.prefix_parse_fns.get(&self.cur_token.ttype).unwrap();
        let mut left_expr = prefix(self);

        while !self.peek_token_is(TokenType::Semicolon) && prec < self.peek_precedence() {
            let infix_fn_key = self.peek_token.ttype.clone();
            let infix = match self.infix_parse_fns.get(&infix_fn_key) {
                Some(func) => func.clone(),
                None => break,
            };

            self.next_token();
            left_expr = infix(self, left_expr);
        }

        return left_expr;
    }

    fn parse_identifier(self: &mut Self) -> ast::Expression {
        return ast::Expression::Identifier {
            name: self.cur_token.literal.clone(),
        };
    }

    fn parse_integer_literal(self: &mut Self) -> ast::Expression {
        let int_lit = self.cur_token.literal.clone().parse::<i64>().unwrap();
        return ast::Expression::IntegerLiteral { value: int_lit };
    }

    fn parse_boolean(self: &mut Self) -> ast::Expression {
        return ast::Expression::Boolean {
            value: self.cur_token_is(TokenType::TRUE),
        };
    }

    fn parse_grouped_expression(self: &mut Self) -> ast::Expression {
        self.next_token();
        let expr = self.parse_expression(Precedence::Lowest);
        if !self.expect_peek(TokenType::RParen) {
            panic!("expected TokenType::RParen");
        }
        return expr;
    }

    fn parse_prefix_expression(self: &mut Self) -> ast::Expression {
        let prefix_operator = self.cur_token.literal.clone();
        self.next_token();
        let expr = self.parse_expression(Precedence::Prefix);
        return ast::Expression::PrefixExpression {
            operator: prefix_operator,
            right: Box::new(expr),
        };
    }

    fn parse_infix_expression(self: &mut Self, left: ast::Expression) -> ast::Expression {
        let infix_operator = self.cur_token.literal.clone();
        let prec = self.cur_precedence();
        self.next_token();
        let rexpr = self.parse_expression(prec);
        return ast::Expression::InfixExpression {
            left: Box::new(left),
            operator: infix_operator,
            right: Box::new(rexpr),
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

    fn peek_precedence(self: &mut Self) -> Precedence {
        match self.precedences.get(&self.peek_token.ttype) {
            Some(prec) => *prec,
            None => Precedence::Lowest,
        }
    }

    fn cur_precedence(self: &mut Self) -> Precedence {
        match self.precedences.get(&self.cur_token.ttype) {
            Some(prec) => *prec,
            None => Precedence::Lowest,
        }
    }
}
