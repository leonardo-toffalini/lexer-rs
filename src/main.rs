use std::{env, fs::File, io, io::Read};

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
    let args: Vec<String> = env::args().collect();
    let mode = &args[1];

    let contents = match mode.as_str() {
        "inline" => &args[2],
        "file" => &read_file_contents(&args[2]).unwrap(),
        m => panic!("Invalid mode: {}, valid modes are 'inline' and 'file'.", m),
    };

    println!("File contents:\n{}", contents);

    let tokens = lexer::lex(&contents).unwrap();

    let mut parser = parser::Parser::new(tokens.clone());
    let program = parser.parse();

    if parser.errors.len() == 0 {
        println!("\nProgram: \n{}", program);
        println!("\nAST: \n{:#?}", program);
    } else {
        let monkey_face = r#"
           __,__
  .--.  .-"     "-.  .--.
 / .. \/  .-. .-.  \/ .. \
| |  '|  /   Y   \  |'  | |
| \   \  \ 0 | 0 /  /   / |
 \ '- ,\.-"""""""-./, -' /
  ''-' /_   ^ ^   _\ '-''
      |  \._   _./  |
      \   \ '~' /   /
       '._ '-=-' _.'
          '-----'
"#;
        println!("\nOops! We ran into some monkey business!");
        println!("{}", monkey_face);
        println!("\nHere are the errors we collected: \n{:#?}", parser.errors);
    }

    Ok(())
}

// Tests
#[cfg(test)]
mod tests {
    use crate::ast;
    use crate::lexer;
    use crate::parser;
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

    #[test]
    fn parse_let_stmt_test() {
        let source = "let foo = 2 + 3;";
        let tokens = lexer::lex(source).unwrap();
        let mut parser = parser::Parser::new(tokens);
        let result = parser.parse();
        let expected = ast::Program {
            statements: vec![ast::Statement::LetStatement {
                name: ast::Expression::Identifier {
                    name: String::from("foo"),
                },
                value: ast::Expression::InfixExpression {
                    left: Box::new(ast::Expression::IntegerLiteral { value: 2 }),
                    operator: ast::Operator::Plus,
                    right: Box::new(ast::Expression::IntegerLiteral { value: 3 }),
                },
            }],
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_return_stmt_test() {
        let source = "return 2 + 3;";
        let tokens = lexer::lex(source).unwrap();
        let mut parser = parser::Parser::new(tokens);
        let result = parser.parse();
        let expected = ast::Program {
            statements: vec![ast::Statement::ReturnStatement {
                value: ast::Expression::InfixExpression {
                    left: Box::new(ast::Expression::IntegerLiteral { value: 2 }),
                    operator: ast::Operator::Plus,
                    right: Box::new(ast::Expression::IntegerLiteral { value: 3 }),
                },
            }],
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_ident_expr_stmt_test() {
        let source = "foo;";
        let tokens = lexer::lex(source).unwrap();
        let mut parser = parser::Parser::new(tokens);
        let result = parser.parse();
        let expected = ast::Program {
            statements: vec![ast::Statement::ExpressionStatement {
                expr: ast::Expression::Identifier {
                    name: String::from("foo"),
                },
            }],
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_int_lit_expr_stmt_test() {
        let source = "12345;";
        let tokens = lexer::lex(source).unwrap();
        let mut parser = parser::Parser::new(tokens);
        let result = parser.parse();
        let expected = ast::Program {
            statements: vec![ast::Statement::ExpressionStatement {
                expr: ast::Expression::IntegerLiteral { value: 12345 },
            }],
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_prefix_minus_op_test() {
        let source = "-12345;";
        let tokens = lexer::lex(source).unwrap();
        let mut parser = parser::Parser::new(tokens);
        let result = parser.parse();
        let expected = ast::Program {
            statements: vec![ast::Statement::ExpressionStatement {
                expr: ast::Expression::PrefixExpression {
                    operator: ast::Operator::Minus,
                    right: Box::new(ast::Expression::IntegerLiteral { value: 12345 }),
                },
            }],
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_prefix_bang_op_test() {
        let source = "!12345;";
        let tokens = lexer::lex(source).unwrap();
        let mut parser = parser::Parser::new(tokens);
        let result = parser.parse();
        let expected = ast::Program {
            statements: vec![ast::Statement::ExpressionStatement {
                expr: ast::Expression::PrefixExpression {
                    operator: ast::Operator::Bang,
                    right: Box::new(ast::Expression::IntegerLiteral { value: 12345 }),
                },
            }],
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_infix_add_minus_test() {
        let source = "1 + 2 - 3;";
        let tokens = lexer::lex(source).unwrap();
        let mut parser = parser::Parser::new(tokens);
        let result = parser.parse();
        let expected = ast::Program {
            statements: vec![ast::Statement::ExpressionStatement {
                expr: ast::Expression::InfixExpression {
                    left: Box::new(ast::Expression::InfixExpression {
                        left: Box::new(ast::Expression::IntegerLiteral { value: 1 }),
                        operator: ast::Operator::Plus,
                        right: Box::new(ast::Expression::IntegerLiteral { value: 2 }),
                    }),
                    operator: ast::Operator::Minus,
                    right: Box::new(ast::Expression::IntegerLiteral { value: 3 }),
                },
            }],
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_true_bool_expr_stmt_test() {
        let source = "true;";
        let tokens = lexer::lex(source).unwrap();
        let mut parser = parser::Parser::new(tokens);
        let result = parser.parse();
        let expected = ast::Program {
            statements: vec![ast::Statement::ExpressionStatement {
                expr: ast::Expression::Boolean { value: true },
            }],
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_false_bool_expr_stmt_test() {
        let source = "false;";
        let tokens = lexer::lex(source).unwrap();
        let mut parser = parser::Parser::new(tokens);
        let result = parser.parse();
        let expected = ast::Program {
            statements: vec![ast::Statement::ExpressionStatement {
                expr: ast::Expression::Boolean { value: false },
            }],
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_op_prec_test() {
        let source = "1 + 2 * 3;";
        let tokens = lexer::lex(source).unwrap();
        let mut parser = parser::Parser::new(tokens);
        let result = parser.parse();
        let expected = ast::Program {
            statements: vec![ast::Statement::ExpressionStatement {
                expr: ast::Expression::InfixExpression {
                    left: Box::new(ast::Expression::IntegerLiteral { value: 1 }),
                    operator: ast::Operator::Plus,
                    right: Box::new(ast::Expression::InfixExpression {
                        left: Box::new(ast::Expression::IntegerLiteral { value: 2 }),
                        operator: ast::Operator::Star,
                        right: Box::new(ast::Expression::IntegerLiteral { value: 3 }),
                    }),
                },
            }],
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_grouped_expr_test() {
        let source = "(1 + 2) * 3;";
        let tokens = lexer::lex(source).unwrap();
        let mut parser = parser::Parser::new(tokens);
        let result = parser.parse();
        let expected = ast::Program {
            statements: vec![ast::Statement::ExpressionStatement {
                expr: ast::Expression::InfixExpression {
                    left: Box::new(ast::Expression::InfixExpression {
                        left: Box::new(ast::Expression::IntegerLiteral { value: 1 }),
                        operator: ast::Operator::Plus,
                        right: Box::new(ast::Expression::IntegerLiteral { value: 2 }),
                    }),
                    operator: ast::Operator::Star,
                    right: Box::new(ast::Expression::IntegerLiteral { value: 3 }),
                },
            }],
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_function_literal_test() {
        let source = "let add = fn(x, y) { return x + y; };";
        let tokens = lexer::lex(source).unwrap();
        let mut parser = parser::Parser::new(tokens);
        let result = parser.parse();
        let expected = ast::Program {
            statements: vec![ast::Statement::LetStatement {
                name: ast::Expression::Identifier {
                    name: String::from("add"),
                },
                value: ast::Expression::FunctionLiteral {
                    parameters: vec![
                        ast::Expression::Identifier {
                            name: String::from("x"),
                        },
                        ast::Expression::Identifier {
                            name: String::from("y"),
                        },
                    ],
                    body: Box::new(ast::Statement::BlockStatement {
                        statements: vec![ast::Statement::ReturnStatement {
                            value: ast::Expression::InfixExpression {
                                left: Box::new(ast::Expression::Identifier {
                                    name: String::from("x"),
                                }),
                                operator: ast::Operator::Plus,
                                right: Box::new(ast::Expression::Identifier {
                                    name: String::from("y"),
                                }),
                            },
                        }],
                    }),
                },
            }],
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_call_expr_test() {
        let source = "add(1, 2 + 3);";
        let tokens = lexer::lex(source).unwrap();
        let mut parser = parser::Parser::new(tokens);
        let result = parser.parse();
        let expected = ast::Program {
            statements: vec![ast::Statement::ExpressionStatement {
                expr: ast::Expression::CallExpression {
                    function: Box::new(ast::Expression::Identifier {
                        name: String::from("add"),
                    }),
                    arguments: vec![
                        ast::Expression::IntegerLiteral { value: 1 },
                        ast::Expression::InfixExpression {
                            left: Box::new(ast::Expression::IntegerLiteral { value: 2 }),
                            operator: ast::Operator::Plus,
                            right: Box::new(ast::Expression::IntegerLiteral { value: 3 }),
                        },
                    ],
                },
            }],
        };
        assert_eq!(result, expected);
    }
}
