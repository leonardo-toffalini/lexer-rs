#[derive(Debug, PartialEq)]
pub enum Node {
    ProgramNode(Program),
    ExpressionNode,
    StatementNode(Statement),
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    LetStatement { name: Expression, value: Expression },
    ReturnStatement { value: Expression },
    ExpressionStatement { expr: Expression },
    BlockStatement { statements: Vec<Statement> },
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    EmptyExpression, // temporary for piecing together the parser
    Identifier {
        name: String,
    },

    IntegerLiteral {
        value: i64,
    },

    PrefixExpression {
        operator: String, // i dont like this being a String
        right: Box<Expression>,
    },

    InfixExpression {
        left: Box<Expression>,
        operator: String, // i dont like this being a String
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
}

#[derive(Debug, PartialEq)]
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
