use crate::ast::Node::{ExpressionNode, ProgramNode, StatementNode};
use crate::ast::{self, Statement};
use crate::ast::{Expression, Program};
use crate::object::Object;

pub fn eval(node: ast::Node) -> Object {
    match node {
        ProgramNode(Program { statements }) => evaluate_statements(statements),

        StatementNode(Statement::ExpressionStatement { expr }) => eval(ExpressionNode(expr)),

        ExpressionNode(Expression::IntegerLiteral { value }) => Object::Integer { value },

        ExpressionNode(_expr) => panic!("Not implemented!"),
        StatementNode(_stmt) => panic!("Not implemented!"),
    }
}

fn evaluate_statements(statements: Vec<Statement>) -> Object {
    let mut result = Object::Null;

    for statement in statements {
        result = eval(StatementNode(statement));
    }

    return result;
}
