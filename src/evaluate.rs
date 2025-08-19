use crate::ast::Node::{ExpressionNode, ProgramNode, StatementNode};
use crate::ast::{self, Operator, Statement};
use crate::ast::{Expression, Program};
use crate::environment::Env;
use crate::object::Object;
use std::mem;

pub fn eval(node: ast::Node, env: &mut Env) -> Object {
    match node {
        ProgramNode(Program { statements }) => evaluate_program(statements, env),

        StatementNode(Statement::ExpressionStatement { expr }) => eval(ExpressionNode(expr), env),
        StatementNode(Statement::BlockStatement { statements }) => {
            evaluate_block_statement(statements, env)
        }
        StatementNode(Statement::ReturnStatement { value }) => {
            let val = eval(ExpressionNode(value), env);
            if matches!(val, Object::Error { .. }) {
                return val;
            }

            Object::ReturnValue {
                value: Box::new(val),
            }
        }
        StatementNode(Statement::LetStatement { name, value }) => {
            let val = eval(ExpressionNode(value), env);
            if matches!(val, Object::Error { .. }) {
                return val;
            }

            match name {
                Expression::Identifier { name } => {
                    env.set(name, val);
                    Object::Null
                }
                _ => Object::Error {
                    message: "Did not find identifier on the right side of let statement"
                        .to_string(),
                },
            }
        }

        ExpressionNode(Expression::IntegerLiteral { value }) => Object::Integer { value },
        ExpressionNode(Expression::Boolean { value }) => native_bool_to_object(value),
        ExpressionNode(Expression::IfExpression {
            condition,
            consequence,
            alternative,
        }) => eval_if_expression(condition, consequence, alternative, env),

        ExpressionNode(Expression::PrefixExpression { operator, right }) => {
            let right = eval(ExpressionNode(*right), env);
            if matches!(right, Object::Error { .. }) {
                return right;
            }

            eval_prefix_expression(operator, right)
        }

        ExpressionNode(Expression::InfixExpression {
            left,
            operator,
            right,
        }) => {
            let left = eval(ExpressionNode(*left), env);
            if matches!(left, Object::Error { .. }) {
                return left;
            }

            let right = eval(ExpressionNode(*right), env);
            if matches!(right, Object::Error { .. }) {
                return right;
            }

            eval_infix_expression(left, operator, right)
        }

        ExpressionNode(Expression::Identifier { name }) => match env.get(name.clone()) {
            Some(val) => val.clone(),
            None => Object::Error {
                message: format!("Identifier not found: {}", name),
            },
        },

        ExpressionNode(expr) => Object::Error {
            message: format!("Not implemented: {}", expr),
        },
        // StatementNode(_stmt) => panic!("Not implemented!"),
    }
}

fn evaluate_program(statements: Vec<Statement>, env: &mut Env) -> Object {
    let mut result = Object::Null;

    for statement in statements {
        result = eval(StatementNode(statement), env);
        if let Object::ReturnValue { value } = result {
            return *value;
        }
        if matches!(result, Object::Error { .. }) {
            return result;
        }
    }

    return result;
}

fn evaluate_block_statement(statements: Vec<Statement>, env: &mut Env) -> Object {
    let mut result = Object::Null;

    for statement in statements {
        result = eval(StatementNode(statement), env);
        if let Object::ReturnValue { value: _value } = &result {
            return result;
        }
        if matches!(result, Object::Error { .. }) {
            return result;
        }
    }

    return result;
}

fn eval_prefix_expression(operator: Operator, right: Object) -> Object {
    match operator {
        Operator::Bang => eval_bang_operator(right),
        Operator::Minus => eval_minus_operator(right),
        _ => Object::Error {
            message: format!("Unrecognized prefix operator: {operator}"),
        },
    }
}

fn eval_infix_expression(left: Object, operator: Operator, right: Object) -> Object {
    match (left, operator, right) {
        (Object::Integer { value: lvalue }, operator, Object::Integer { value: rvalue }) => {
            eval_infix_int_expression(lvalue, operator, rvalue)
        }
        (left, Operator::Eq, right) => native_bool_to_object(left == right),
        (left, Operator::Neq, right) => native_bool_to_object(left != right),
        (left, _, right) => {
            if mem::discriminant(&left) != mem::discriminant(&right) {
                Object::Error {
                    message: format!(
                        "Type mismatch: {} {} {}",
                        left.mytype(),
                        operator,
                        right.mytype()
                    ),
                }
            } else {
                Object::Error {
                    message: format!(
                        "Unknown operator: {} {} {}",
                        left.mytype(),
                        operator,
                        right.mytype()
                    ),
                }
            }
        }
    }
}

fn native_bool_to_object(value: bool) -> Object {
    match value {
        true => Object::new_true(),
        false => Object::new_false(),
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

fn eval_if_expression(
    condition: Box<Expression>,
    consequence: Box<Statement>,
    alternative: Option<Box<Statement>>,
    env: &mut Env,
) -> Object {
    let cond_obj = eval(ExpressionNode(*condition), env);
    if matches!(cond_obj, Object::Error { .. }) {
        return cond_obj;
    }

    let cons_obj = eval(StatementNode(*consequence), env);

    if is_truthy(cond_obj) {
        return cons_obj;
    }

    match alternative {
        None => Object::Null,
        Some(stmt) => {
            return eval(StatementNode(*stmt), env);
        }
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
        _ => Object::Error {
            message: String::from("Unrecognized infix int operator: {operator}"),
        },
    }
}
