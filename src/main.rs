use std::{env, fs::File, io, io::Read};

pub mod ast;
pub mod environment;
pub mod evaluate;
pub mod lexer;
pub mod object;
pub mod parser;
pub mod repl;
pub mod token;

fn read_file_contents(path: &str) -> Result<String, io::Error> {
    let mut file = File::open(path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        return Err("Usage: lexer-rs <mode> [content/file]\nModes: repl, inline, file".to_string());
    }

    let mode = &args[1];

    if mode.as_str() == "repl" {
        repl::run();
        return Ok(());
    }

    if args.len() < 3 {
        return Err("Content or file path required for inline/file modes".to_string());
    }

    let contents = match mode.as_str() {
        "inline" => args[2].clone(),
        "file" => read_file_contents(&args[2])
            .map_err(|e| format!("Failed to read file '{}': {}", &args[2], e))?,
        m => {
            return Err(format!(
                "Invalid mode: '{}'. Valid modes are 'repl', 'inline', and 'file'.",
                m
            ));
        }
    };

    println!("File contents:\n{}", contents);

    let tokens = lexer::lex(&contents).map_err(|e| format!("Lexing error: {}", e))?;

    let mut parser = parser::Parser::new(tokens.clone());
    let program = parser.parse();

    if parser.errors.is_empty() {
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

    let mut env = environment::Env::new();
    let eval_result = evaluate::eval(ast::Node::ProgramNode(program), &mut env);
    println!("\nEval result:\n{:#?}", eval_result);

    Ok(())
}

// Tests
#[cfg(test)]
mod tests {
    use crate::evaluate::eval;
    use crate::lexer;
    use crate::object::Object;
    use crate::parser;
    use crate::token::{Token, TokenType};
    use crate::{ast, environment};

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

    #[test]
    fn eval_int_lit_test() {
        let source = "123;";
        let tokens = lexer::lex(source).unwrap();
        let mut parser = parser::Parser::new(tokens);
        let program = parser.parse();
        let mut env = environment::Env::new();
        let result = eval(ast::Node::ProgramNode(program), &mut env);
        let expected = Object::Integer { value: 123 };
        assert_eq!(result, expected);
    }

    #[test]
    fn eval_prefix_bang_test() {
        let sources = vec!["!true;", "!false;", "!123;", "!0;"];
        let expecteds = vec![
            Object::Boolean { value: false },
            Object::Boolean { value: true },
            Object::Boolean { value: false },
            Object::Boolean { value: true },
        ];

        for (source, expected) in sources.iter().zip(expecteds.iter()) {
            let tokens = lexer::lex(source).unwrap();
            let mut parser = parser::Parser::new(tokens);
            let program = parser.parse();
            let mut env = environment::Env::new();
            let result = eval(ast::Node::ProgramNode(program), &mut env);
            assert_eq!(result, *expected);
        }
    }

    #[test]
    fn eval_prefix_minus_test() {
        let sources = vec!["-56;", "--42;", "---37;", "-3.14;"];
        let expecteds = vec![
            Object::Integer { value: -56 },
            Object::Integer { value: 42 },
            Object::Integer { value: -37 },
            Object::Float { value: -3.14 },
        ];

        for (source, expected) in sources.iter().zip(expecteds.iter()) {
            let tokens = lexer::lex(source).unwrap();
            let mut parser = parser::Parser::new(tokens);
            let program = parser.parse();
            let mut env = environment::Env::new();
            let result = eval(ast::Node::ProgramNode(program), &mut env);
            assert_eq!(result, *expected);
        }
    }

    #[test]
    fn eval_infix_int_test() {
        let sources = vec![
            "1 + 1;", "3 - 1;", "5 * 8;", "12 / 3;", "1 == 2;", "1 != 2;", "1 < 2;", "1 <= 2;",
            "1 > 2;", "1 >= 2;",
        ];
        let expecteds = vec![
            Object::Integer { value: 2 },
            Object::Integer { value: 2 },
            Object::Integer { value: 40 },
            Object::Integer { value: 4 },
            Object::Boolean { value: false },
            Object::Boolean { value: true },
            Object::Boolean { value: true },
            Object::Boolean { value: true },
            Object::Boolean { value: false },
            Object::Boolean { value: false },
        ];

        for (source, expected) in sources.iter().zip(expecteds.iter()) {
            let tokens = lexer::lex(source).unwrap();
            let mut parser = parser::Parser::new(tokens);
            let program = parser.parse();
            let mut env = environment::Env::new();
            let result = eval(ast::Node::ProgramNode(program), &mut env);
            assert_eq!(result, *expected);
        }
    }

    #[test]
    fn eval_infix_float_test() {
        let sources = vec![
            "0.1 + 0.1;",
            "0.3 - 0.1;",
            "0.5 * 0.8;",
            "0.12 / 0.3;",
            "0.1 == 0.2;",
            "0.1 != 0.2;",
            "0.1 < 0.2;",
            "0.1 <= 0.2;",
            "0.1 > 0.2;",
            "0.1 >= 0.2;",
        ];
        let expecteds = vec![
            Object::Float { value: 0.2 },
            Object::Float { value: 0.2 },
            Object::Float { value: 0.4 },
            Object::Float { value: 0.4 },
            Object::Boolean { value: false },
            Object::Boolean { value: true },
            Object::Boolean { value: true },
            Object::Boolean { value: true },
            Object::Boolean { value: false },
            Object::Boolean { value: false },
        ];

        for (source, expected) in sources.iter().zip(expecteds.iter()) {
            let tokens = lexer::lex(source).unwrap();
            let mut parser = parser::Parser::new(tokens);
            let program = parser.parse();
            let mut env = environment::Env::new();
            let result = eval(ast::Node::ProgramNode(program), &mut env);
            assert_eq!(result, *expected);
        }
    }

    #[test]
    fn eval_infix_test() {
        let sources = vec![
            "true == true;",
            "true == false;",
            "true != true;",
            "true != false;",
        ];
        let expecteds = vec![
            Object::Boolean { value: true },
            Object::Boolean { value: false },
            Object::Boolean { value: false },
            Object::Boolean { value: true },
        ];

        for (source, expected) in sources.iter().zip(expecteds.iter()) {
            let tokens = lexer::lex(source).unwrap();
            let mut parser = parser::Parser::new(tokens);
            let program = parser.parse();
            let mut env = environment::Env::new();
            let result = eval(ast::Node::ProgramNode(program), &mut env);
            assert_eq!(result, *expected);
        }
    }

    #[test]
    fn eval_if_expression_test() {
        let sources = vec![
            "if (true) { 10 }",
            "if (false) { 10 }",
            "if (1) { 10 }",
            "if (1 < 2) { 10 }",
            "if (1 > 2) { 10 }",
            "if (1 > 2) { 10 } else { 20 }",
            "if (1 < 2) { 10 } else { 20 }",
        ];

        let expecteds = vec![
            Object::Integer { value: 10 },
            Object::Null,
            Object::Integer { value: 10 },
            Object::Integer { value: 10 },
            Object::Null,
            Object::Integer { value: 20 },
            Object::Integer { value: 10 },
        ];

        for (source, expected) in sources.iter().zip(expecteds.iter()) {
            let tokens = lexer::lex(source).unwrap();
            let mut parser = parser::Parser::new(tokens);
            let program = parser.parse();
            let mut env = environment::Env::new();
            let result = eval(ast::Node::ProgramNode(program), &mut env);
            assert_eq!(result, *expected);
        }
    }

    #[test]
    fn eval_return_statement_test() {
        let sources = vec![
            "return 10;",
            "return 10; 9;",
            "return 2 * 5; 9;",
            "9; return 2 * 5; 9;",
            "if (10 > 1) { if (10 > 1) { return 10; } return 1; }",
        ];

        let expecteds = vec![
            Object::Integer { value: 10 },
            Object::Integer { value: 10 },
            Object::Integer { value: 10 },
            Object::Integer { value: 10 },
            Object::Integer { value: 10 },
        ];

        for (source, expected) in sources.iter().zip(expecteds.iter()) {
            let tokens = lexer::lex(source).unwrap();
            let mut parser = parser::Parser::new(tokens);
            let program = parser.parse();
            let mut env = environment::Env::new();
            let result = eval(ast::Node::ProgramNode(program), &mut env);
            assert_eq!(result, *expected);
        }
    }
}
