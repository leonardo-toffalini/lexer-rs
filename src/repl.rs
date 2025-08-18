use std::io::{self, BufRead, Write};

use crate::{ast, environment, evaluate, lexer, parser};

pub fn run() -> () {
    println!("Hello from the REPL!\nType 'exit' to quit the program.");
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut env = environment::Env::new();

    loop {
        let mut line = String::new();
        print!("> ");
        stdout.flush().unwrap();
        stdin.lock().read_line(&mut line).unwrap();
        line = line.trim_end().to_string();

        if "exit" == line {
            return;
        }

        let tokens = lexer::lex(&line).unwrap();

        let mut parser = parser::Parser::new(tokens.clone());
        let program = parser.parse();

        if parser.errors.len() != 0 {
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

        let eval_result = evaluate::eval(ast::Node::ProgramNode(program), &mut env);
        println!("{}", eval_result);
    }
}
