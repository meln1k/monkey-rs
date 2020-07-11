use crate::ast::Node::Prog;
use crate::ast::Statement::Block;
use crate::ast::*;
use crate::lexer::lexer::Lexer;
use crate::object::Numeric::{Float, Integer};
use crate::object::{Object, FALSE, TRUE};
use crate::parser::Parser;

type EvalResult = Result<Object, String>;

pub fn eval(node: Node) -> EvalResult {
    match node {
        Node::Prog(Program { statements }) => eval_statements(statements),
        other => todo!(),
    }
}

fn eval_statements(statements: Vec<Statement>) -> EvalResult {
    let mut obj: Option<Object> = None;

    for statement in statements {
        obj = Some(eval_statement(statement)?);
    }

    obj.ok_or("no statements provided".to_string())
}

fn eval_statement(statement: Statement) -> EvalResult {
    match statement {
        Statement::Expr(ExpressionStatement { expression }) => eval_expression(expression),
        Statement::Block(BlockStatement { statements }) => eval_statements(statements),
        _ => todo!(),
    }
}

fn eval_expression(expression: Expression) -> EvalResult {
    match expression {
        Expression::IntegerLiteral(int) => Ok(Object::Num(Integer(int))),
        Expression::FloatLiteral(float) => Ok(Object::Num(Float(float))),
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
        Expression::IfExpression {
            condition,
            consequence,
            alternative,
        } => {
            let cond = eval_expression(*condition)?;
            match cond {
                Object::Boolean(true) => eval_statement(Block(*consequence)),
                Object::Boolean(false) => match alternative {
                    Some(block) => eval_statement(Block(*block)),
                    None => Ok(Object::Null),
                },
                other => Err(format!(
                    "if expression can only use bool as a condition, but got {}",
                    other
                )),
            }
        }
        _ => todo!(),
    }
}

fn eval_prefix_expression(operator: PrefixOperator, right: Object) -> EvalResult {
    match operator {
        PrefixOperator::BANG => eval_bang_operator_expression(right),
        PrefixOperator::MINUS => eval_minus_operator_expression(right),
    }
}

fn eval_infix_expression(operator: InfixOperator, left: Object, right: Object) -> EvalResult {
    match (left, right) {
        (Object::Num(l), Object::Num(r)) => match operator {
            InfixOperator::PLUS => Ok(Object::Num(l + r)),
            InfixOperator::MINUS => Ok(Object::Num(l - r)),
            InfixOperator::ASTERISK => Ok(Object::Num(l * r)),
            InfixOperator::SLASH => {
                match r {
                    Integer(0) => Err(format!("division by 0: {}/{}", l, r)),
                    Float(0.0) => Err(format!("division by 0: {}/{}", l, r)),
                    _ => Ok(Object::Num(l / r))
                }
            },
            InfixOperator::LT => Ok(native_bool_to_boolean_object(l < r)),
            InfixOperator::GT => Ok(native_bool_to_boolean_object(l > r)),
            InfixOperator::EQ => Ok(native_bool_to_boolean_object(l == r)),
            InfixOperator::NOT_EQ => Ok(native_bool_to_boolean_object(l != r)),
        },
        (left, right) => match operator {
            InfixOperator::EQ => Ok(native_bool_to_boolean_object(left == right)),
            InfixOperator::NOT_EQ => Ok(native_bool_to_boolean_object(left != right)),
            other => Err(format!(
                "can't use {} operator with {}{}",
                other, left, right
            )),
        },
    }
}

fn eval_bang_operator_expression(right: Object) -> EvalResult {
    match right {
        Object::Boolean(true) => Ok(FALSE),
        Object::Boolean(false) => Ok(TRUE),
        other => Err(format!("can't use ! operator with {}", other)),
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
        Object::Num(int) => Ok(Object::Num(-int)),
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
fn test_eval_float_expr() {
    struct Test<'a>(&'a str, f64);

    let tests = vec![
        Test("5.", 5.),
        Test("10.0", 10.),
        Test("-5.2", -5.2),
        Test("-10.3", -10.3),
        Test("5. + 5.0 + 5.000 + 5 - 10", 10.),
        Test("2. * 2. * 2. * 2 * 2", 32.),
        Test("-50.0 + 100 + -50", 0.),
        Test("(5. + 10 * 2 + 15. / 3.) * 2 + -10", 50.),
    ];

    for Test(input, expected) in tests {
        let evaluated = test_eval(input).expect("evaluation failed");
        test_float_object(evaluated, expected);
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
        Test("!!true", true),
        Test("!!false", false),
    ];

    for Test(input, expected) in tests {
        let evaluated = test_eval(input).expect("evaluation failed");
        test_boolean_object(evaluated, expected);
    }
}

#[test]
fn test_if_else_expressions() {
    struct Test<'a>(&'a str, Option<i64>);

    let tests = vec![
        Test("if (true) { 10 }", Some(10)),
        Test("if (false) { 10 }", None),
        Test("if (1 < 2) { 10 }", Some(10)),
        Test("if (1 > 2) { 10 }", None),
        Test("if (1 > 2) { 10 } else { 20 }", Some(20)),
        Test("if (1 < 2) { 10 } else { 20 }", Some(10)),
    ];

    for Test(input, expected) in tests {
        let evaluated = test_eval(input).expect("evaluation failed");

        match expected {
            Some(int) => test_integer_object(evaluated, int),
            None => assert_eq!(evaluated, Object::Null),
        }
    }
}

fn test_eval(input: &str) -> EvalResult {
    let lexer = Lexer::new(input);
    let parser = Parser::new(lexer);
    let program = parser.parse_program().map_err(|e| e.join(", "))?;

    eval(Prog(program))
}

fn test_float_object(obj: Object, expected: f64) {
    match obj {
        Object::Num(n) => match n {
            Float(i) =>  assert_eq!(i, expected),
            other => panic!("expected Integer but got {:?}", other),
        },
        other => panic!("expected Integer but got {:?}", other),
    }
}

fn test_integer_object(obj: Object, expected: i64) {
    match obj {
        Object::Num(n) => match n {
            Integer(i) =>  assert_eq!(i, expected),
            other => panic!("expected Integer but got {:?}", other),
        },
        other => panic!("expected Integer but got {:?}", other),
    }
}

fn test_boolean_object(obj: Object, expected: bool) {
    match obj {
        Object::Boolean(b) => assert_eq!(b, expected),
        other => panic!("expected Integer but got {:?}", other),
    }
}
