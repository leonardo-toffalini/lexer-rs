#[derive(Debug)]
pub enum Node {
    ProgramNode(Program),
    ExpressionNode,
    IdentifierNode(Identifier),
    StatementNode(Statement),
}

#[derive(Debug)]
pub enum Statement {
    LetStatement { name: Identifier, value: Expression },
}

#[derive(Debug)]
pub enum Expression {
    EmptyExpression,
}

#[derive(Debug)]
pub struct Program {
    statements: Vec<Statement>,
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

#[derive(Debug)]
pub struct Identifier {
    pub value: String,
}
