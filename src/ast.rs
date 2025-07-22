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
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    EmptyExpression, // temporary for piecing together the parser
    Identifier { name: String },
    IntegerLiteral { value: i64 },
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
