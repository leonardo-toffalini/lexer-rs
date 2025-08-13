use crate::ast::Node::{ExpressionNode, ProgramNode, StatementNode};
use crate::ast::{self, Operator, Statement};
use crate::ast::{Expression, Program};
use crate::object::Object;

static TRUE: Object = Object::Boolean { value: true };
static FALSE: Object = Object::Boolean { value: false };
static NULL: Object = Object::Null;

pub fn eval(node: ast::Node) -> Object {
    match node {
        ProgramNode(Program { statements }) => evaluate_statements(statements),

        StatementNode(Statement::ExpressionStatement { expr }) => eval(ExpressionNode(expr)),

        ExpressionNode(Expression::IntegerLiteral { value }) => Object::Integer { value },
        ExpressionNode(Expression::Boolean { value }) => native_bool_to_object(value),
        ExpressionNode(Expression::PrefixExpression { operator, right }) => {
            let right = eval(ExpressionNode(*right));
            eval_prefix_expression(operator, right)
        }

        ExpressionNode(Expression::InfixExpression {
            left,
            operator,
            right,
        }) => {
            let left = eval(ExpressionNode(*left));
            let right = eval(ExpressionNode(*right));
            eval_infix_expression(left, operator, right)
        }

        ExpressionNode(_expr) => panic!("Not implemented!"),
        StatementNode(_stmt) => panic!("Not implemented!"),
    }
}

fn evaluate_statements(statements: Vec<Statement>) -> Object {
    let mut result = NULL;

    for statement in statements {
        result = eval(StatementNode(statement));
    }

    return result;
}

fn eval_prefix_expression(operator: Operator, right: Object) -> Object {
    match operator {
        Operator::Bang => eval_bang_operator(right),
        Operator::Minus => eval_minus_operator(right),
        _ => NULL,
    }
}

fn eval_infix_expression(left: Object, operator: Operator, right: Object) -> Object {
    match (left, right) {
        (Object::Integer { value: lvalue }, Object::Integer { value: rvalue }) => {
            eval_infix_int_expression(lvalue, operator, rvalue)
        }
        _ => NULL,
    }
}

fn native_bool_to_object(value: bool) -> Object {
    match value {
        true => TRUE,
        false => FALSE,
    }
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        Object::Boolean { value } => value,
        Object::Integer { value: 0 } => false,
        Object::Null => false,
        _ => true,
    }
}

fn eval_bang_operator(right: Object) -> Object {
    native_bool_to_object(!is_truthy(right))
}

fn eval_minus_operator(right: Object) -> Object {
    match right {
        Object::Integer { value } => Object::Integer { value: -value },
        _ => panic!("Minus prefix oeprator used on not integer type"),
    }
}

fn eval_infix_int_expression(lvalue: i64, operator: Operator, rvalue: i64) -> Object {
    match operator {
        Operator::Plus => Object::Integer {
            value: lvalue + rvalue,
        },
        Operator::Minus => Object::Integer {
            value: lvalue - rvalue,
        },
        Operator::Star => Object::Integer {
            value: lvalue * rvalue,
        },
        Operator::Slash => Object::Integer {
            value: lvalue / rvalue,
        },
        Operator::Eq => native_bool_to_object(lvalue == rvalue),
        Operator::Neq => native_bool_to_object(lvalue != rvalue),
        Operator::Lt => native_bool_to_object(lvalue < rvalue),
        Operator::Le => native_bool_to_object(lvalue <= rvalue),
        Operator::Gt => native_bool_to_object(lvalue > rvalue),
        Operator::Ge => native_bool_to_object(lvalue >= rvalue),
        _ => NULL,
    }
}
