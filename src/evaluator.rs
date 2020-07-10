use crate::ast::Node::Prog;
use crate::ast::*;
use crate::lexer::lexer::Lexer;
use crate::object::{Object, TRUE, FALSE};
use crate::parser::Parser;

type EvalResult<T> = Result<T, String>;

pub fn eval(node: Node) -> EvalResult<Object> {
    match node {
        Node::Prog(program) => eval_program(program),
        other => todo!(),
    }
}

fn eval_program(program: Program) -> EvalResult<Object> {
    let mut obj: Option<Object> = None;

    for statement in program.statements {
        obj = Some(eval_statement(statement)?);
    }

    obj.ok_or("no statements provided".to_string())
}

fn eval_statement(statement: Statement) -> EvalResult<Object> {
    match statement {
        Statement::Expr(ExpressionStatement { expression }) => match expression {
            Expression::IntegerLiteral(int) => Ok(Object::Integer(int)),
            Expression::Boolean(true) => Ok(TRUE),
            Expression::Boolean(false) => Ok(FALSE),
            _ => todo!(),
        },
        _ => todo!(),
    }
}

#[test]
fn test_eval_integer_expr() {
    struct Test<'a>(&'a str, i64);

    let tests = vec![Test("5", 5), Test("10", 10)];

    for Test(input, expected) in tests {
        let evaluated = test_eval(input).expect("evaluation failed");
        test_integer_object(evaluated, expected);
    }
}

#[test]
fn test_eval_boolean_expr() {
    struct Test<'a>(&'a str, bool);

    let tests = vec![Test("true", true), Test("false", false)];

    for Test(input, expected) in tests {
        let evaluated = test_eval(input).expect("evaluation failed");
        test_boolean_object(evaluated, expected);
    }
}

fn test_eval(input: &str) -> EvalResult<Object> {
    let lexer = Lexer::new(input);
    let parser = Parser::new(lexer);
    let program = parser.parse_program().map_err(|e| e.join(", "))?;

    eval(Prog(program))
}

fn test_integer_object(obj: Object, expected: i64) {
    match obj {
        Object::Integer(int) => assert_eq!(int, expected),
        other => panic!("expected Integer but got {:?}", other),
    }
}

fn test_boolean_object(obj: Object, expected: bool) {
    match obj {
        Object::Boolean(b) => assert_eq!(b, expected),
        other => panic!("expected Integer but got {:?}", other),
    }
}
