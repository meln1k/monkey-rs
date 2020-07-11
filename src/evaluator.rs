use crate::ast::Node::Prog;
use crate::ast::*;
use crate::lexer::lexer::Lexer;
use crate::object::{Object, FALSE, TRUE};
use crate::parser::Parser;

type EvalResult = Result<Object, String>;

pub fn eval(node: Node) -> EvalResult {
    match node {
        Node::Prog(program) => eval_program(program),
        other => todo!(),
    }
}

fn eval_program(program: Program) -> EvalResult {
    let mut obj: Option<Object> = None;

    for statement in program.statements {
        obj = Some(eval_statement(statement)?);
    }

    obj.ok_or("no statements provided".to_string())
}

fn eval_statement(statement: Statement) -> EvalResult {
    match statement {
        Statement::Expr(ExpressionStatement { expression }) => eval_expression(expression),
        _ => todo!(),
    }
}

fn eval_expression(expression: Expression) -> EvalResult {
    match expression {
        Expression::IntegerLiteral(int) => Ok(Object::Integer(int)),
        Expression::Boolean(value) => Ok(native_bool_to_boolean_object(value)),
        Expression::PrefixExpression { operator, expr } => {
            let right = eval_expression(*expr)?;
            eval_prefix_expression(operator, right)
        }
        Expression::InfixExpression {
            left,
            operator,
            right,
        } => {
            let left = eval_expression(*left)?;
            let right = eval_expression(*right)?;
            eval_infix_expression(operator, left, right)
        }
        _ => todo!(),
    }
}

fn eval_prefix_expression(operator: PrefixOperator, right: Object) -> EvalResult {
    match operator {
        PrefixOperator::BANG => Ok(eval_bang_operator_expression(right)),
        PrefixOperator::MINUS => eval_minus_operator_expression(right),
    }
}

fn eval_infix_expression(operator: InfixOperator, left: Object, right: Object) -> EvalResult {
    match (left, right) {
        (Object::Integer(lint), Object::Integer(rint)) => match operator {
            InfixOperator::PLUS => Ok(Object::Integer(lint + rint)),
            InfixOperator::MINUS => Ok(Object::Integer(lint - rint)),
            InfixOperator::ASTERISK => Ok(Object::Integer(lint * rint)),
            InfixOperator::SLASH => Ok(Object::Integer(lint / rint)),
            InfixOperator::LT => Ok(native_bool_to_boolean_object(lint < rint)),
            InfixOperator::GT => Ok(native_bool_to_boolean_object(lint > rint)),
            InfixOperator::EQ => Ok(native_bool_to_boolean_object(lint == rint)),
            InfixOperator::NOT_EQ => Ok(native_bool_to_boolean_object(lint != rint)),
        },
        (left, right) => match operator {
            InfixOperator::EQ => Ok(native_bool_to_boolean_object(left == right)),
            InfixOperator::NOT_EQ => Ok(native_bool_to_boolean_object(left != right)),
            other => Err(format!("can't use {} operator with {}{}", other, left, right)),
        },
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        TRUE => FALSE,
        FALSE => TRUE,
        Object::Null => TRUE,
        _ => FALSE,
    }
}

fn native_bool_to_boolean_object(input: bool) -> Object {
    match input {
        true => TRUE,
        false => FALSE,
    }
}

fn eval_minus_operator_expression(right: Object) -> EvalResult {
    match right {
        Object::Integer(int) => Ok(Object::Integer(-int)),
        other => Err(format!("can't use - operator with {}", other)),
    }
}

#[test]
fn test_eval_integer_expr() {
    struct Test<'a>(&'a str, i64);

    let tests = vec![
        Test("5", 5),
        Test("10", 10),
        Test("-5", -5),
        Test("-10", -10),
        Test("5 + 5 + 5 + 5 - 10", 10),
        Test("2 * 2 * 2 * 2 * 2", 32),
        Test("-50 + 100 + -50", 0),
        Test("5 * 2 + 10", 20),
        Test("5 + 2 * 10", 25),
        Test("20 + 2 * -10", 0),
        Test("50 / 2 * 2 + 10", 60),
        Test("2 * (5 + 10)", 30),
        Test("3 * 3 * 3 + 10", 37),
        Test("3 * (3 * 3) + 10", 37),
        Test("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
    ];

    for Test(input, expected) in tests {
        let evaluated = test_eval(input).expect("evaluation failed");
        test_integer_object(evaluated, expected);
    }
}

#[test]
fn test_eval_boolean_expr() {
    struct Test<'a>(&'a str, bool);

    let tests = vec![
        Test("true", true),
        Test("false", false),
        Test("1 < 2", true),
        Test("1 > 2", false),
        Test("1 < 1", false),
        Test("1 > 1", false),
        Test("1 == 1", true),
        Test("1 != 1", false),
        Test("1 == 2", false),
        Test("1 != 2", true),
        Test("true == true", true),
        Test("false == false", true),
        Test("true == false", false),
        Test("true != false", true),
        Test("false != true", true),
        Test("(1 < 2) == true", true),
        Test("(1 < 2) == false", false),
        Test("(1 > 2) == true", false),
        Test("(1 > 2) == false", true),
    ];

    for Test(input, expected) in tests {
        let evaluated = test_eval(input).expect("evaluation failed");
        test_boolean_object(evaluated, expected);
    }
}

#[test]
fn test_bang_operator() {
    struct Test<'a>(&'a str, bool);

    let tests = vec![
        Test("!true", false),
        Test("!false", true),
        Test("!5", false),
        Test("!!true", true),
        Test("!!false", false),
        Test("!!5", true),
    ];

    for Test(input, expected) in tests {
        let evaluated = test_eval(input).expect("evaluation failed");
        test_boolean_object(evaluated, expected);
    }
}

fn test_eval(input: &str) -> EvalResult {
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
