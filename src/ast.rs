use std::fmt;

pub enum Node {
    ProgramNode(Program),
    StatementNode(Statement),
    ExpressionNode(Expression),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Star,
    Slash,
    Bang,
    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    LetStatement { name: Expression, value: Expression },
    ReturnStatement { value: Expression },
    ExpressionStatement { expr: Expression },
    BlockStatement { statements: Vec<Statement> },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Identifier {
        name: String,
    },

    IntegerLiteral {
        value: i64,
    },

    FloatLiteral {
        value: f64,
    },

    StringLiteral {
        value: String,
    },

    PrefixExpression {
        operator: Operator,
        right: Box<Expression>,
    },

    InfixExpression {
        left: Box<Expression>,
        operator: Operator,
        right: Box<Expression>,
    },

    Boolean {
        value: bool,
    },

    IfExpression {
        condition: Box<Expression>,
        consequence: Box<Statement>,
        alternative: Option<Box<Statement>>,
    },

    FunctionLiteral {
        parameters: Vec<Expression>,
        body: Box<Statement>,
    },

    CallExpression {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            statements: Vec::new(),
        }
    }

    pub fn push(self: &mut Self, stmt: Statement) -> () {
        self.statements.push(stmt);
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, item) in self.statements.iter().enumerate() {
            if i > 0 {
                writeln!(f)?;
            }
            write!(f, "{}", item)?;
        }
        Ok(())
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operator::Plus => write!(f, "+"),
            Operator::Minus => write!(f, "-"),
            Operator::Star => write!(f, "*"),
            Operator::Slash => write!(f, "/"),
            Operator::Bang => write!(f, "!"),
            Operator::Eq => write!(f, "=="),
            Operator::Neq => write!(f, "!="),
            Operator::Lt => write!(f, "<"),
            Operator::Le => write!(f, "<="),
            Operator::Gt => write!(f, ">"),
            Operator::Ge => write!(f, ">="),
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::LetStatement { name, value } => write!(f, "let {} = {};", name, value)?,
            Statement::ReturnStatement { value } => write!(f, "return {};", value)?,
            Statement::BlockStatement { statements } => write!(
                f,
                "{{{}}}",
                statements
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<_>>()
                    .join("\n")
            )?,
            Statement::ExpressionStatement { expr } => write!(f, "{};", expr)?,
        }
        return Ok(());
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Identifier { name } => write!(f, "{}", name)?,
            Expression::IntegerLiteral { value } => write!(f, "{}", value)?,
            Expression::FloatLiteral { value } => write!(f, "{}", value)?,
            Expression::StringLiteral { value } => write!(f, "{}", value)?,
            Expression::PrefixExpression { operator, right } => {
                write!(f, "({}{})", operator, right)?
            }
            Expression::InfixExpression {
                left,
                operator,
                right,
            } => write!(f, "({} {} {})", left, operator, right)?,
            Expression::Boolean { value } => write!(f, "({})", value)?,
            Expression::IfExpression {
                condition,
                consequence,
                alternative,
            } => match alternative {
                None => write!(f, "if ({}) ({})", condition, consequence)?,
                Some(alternative) => {
                    write!(f, "if ({}) {} else {}", condition, consequence, alternative)?
                }
            },
            Expression::FunctionLiteral { parameters, body } => {
                write!(
                    f,
                    "fn ({}) {}",
                    parameters
                        .iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<_>>()
                        .join(", "),
                    body
                )?;
            }
            Expression::CallExpression {
                function,
                arguments,
            } => write!(
                f,
                "{}({})",
                function,
                arguments
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            )?,
        }
        return Ok(());
    }
}
