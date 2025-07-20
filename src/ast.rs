#[derive(Debug, PartialEq)]
pub enum Node {
    ProgramNode(Program),
    ExpressionNode,
    IdentifierNode(Identifier),
    StatementNode(Statement),
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    LetStatement { name: Identifier, value: Expression },
    ReturnStatement { value: Expression },
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    EmptyExpression,
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

#[derive(Debug, PartialEq)]
pub struct Identifier {
    pub value: String,
}
