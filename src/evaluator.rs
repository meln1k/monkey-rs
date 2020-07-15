use crate::ast::Node::Prog;
use crate::ast::Statement::Block;
use crate::ast::*;
use crate::environment::Environment;
use crate::evaluator::EvalError::{Error, NonLocalReturnControl};
use crate::lexer::lexer::Lexer;
use crate::object::Numeric::{Float, Integer};
use crate::object::{Function, Value, FALSE, TRUE};
use crate::parser::Parser;
use core::fmt;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug)]
pub enum EvalError {
    NonLocalReturnControl(Rc<Value>),
    Error(String),
}

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Error(string) => write!(f, "{}", string),
            NonLocalReturnControl(..) => panic!("NonLocalReturnControl leaked into the error"),
        }
    }
}

type EvalResult = Result<Rc<Value>, EvalError>;

pub fn eval(node: Node, environment: Rc<RefCell<Environment>>) -> EvalResult {
    match node {
        Node::Prog(Program { ref statements }) => eval_statements(statements, environment),
        other => todo!(),
    }
}

fn eval_statements(
    statements: &Vec<Statement>,
    environment: Rc<RefCell<Environment>>,
) -> EvalResult {
    let mut obj: Option<Rc<Value>> = None;

    for statement in statements {
        match eval_statement(statement, Rc::clone(&environment)) {
            Ok(object) => {
                obj = Some(object);
            }
            Err(NonLocalReturnControl(object)) => {
                obj = Some(object);
                break;
            }
            err => {
                err?;
            }
        }
    }

    Ok(obj.unwrap_or(Rc::new(Value::Null)))
}

fn eval_statement(statement: &Statement, environment: Rc<RefCell<Environment>>) -> EvalResult {
    match statement {
        Statement::Expr(ExpressionStatement { expression }) => {
            eval_expression(expression, environment)
        }
        Statement::Block(BlockStatement { statements }) => eval_statements(statements, environment),
        Statement::Return(ReturnStatement { return_value }) => {
            let value = eval_expression(return_value, environment)?;
            Err(NonLocalReturnControl(value))
        }
        Statement::Let(LetStatement { name, value }) => {
            let eval_value = eval_expression(value, Rc::clone(&environment))?;
            Ok(environment.borrow_mut().set(name.clone(), eval_value))
        }
    }
}

fn eval_expression(expression: &Expression, environment: Rc<RefCell<Environment>>) -> EvalResult {
    match expression {
        Expression::IntegerLiteral(int) => Ok(Rc::new(Value::Num(Integer(*int)))),
        Expression::FloatLiteral(float) => Ok(Rc::new(Value::Num(Float(*float)))),
        Expression::Boolean(value) => Ok(native_bool_to_boolean_object(*value)),
        Expression::PrefixExpression { operator, expr } => {
            let right = eval_expression(expr, environment)?;
            eval_prefix_expression(operator, right)
        }
        Expression::InfixExpression {
            left,
            operator,
            right,
        } => {
            let left = eval_expression(left, Rc::clone(&environment))?;
            let right = eval_expression(right, environment)?;
            eval_infix_expression(operator, left, right)
        }
        Expression::IfExpression {
            condition,
            consequence,
            alternative,
        } => {
            let cond = &*eval_expression(condition, Rc::clone(&environment))?;
            match cond {
                Value::Boolean(true) => eval_statements(&consequence.statements, environment),
                Value::Boolean(false) => match alternative {
                    Some(alt) => eval_statements(&alt.statements, environment),
                    None => Ok(Rc::new(Value::Null)),
                },
                other => Err(Error(format!(
                    "type mismatch: expected bool, but got {}",
                    other
                ))),
            }
        }
        Expression::Ident(name) => {
            let env = &*environment;
            match env.borrow().get(&name) {
                Some(obj) => Ok(obj),
                None => Err(Error(format!("identifier not found: {}", name))),
            }
        }
        Expression::FunctionLiteral { parameters, body } => {
            Ok(Rc::new(Value::Func(Rc::new(Function {
                parameters: parameters.clone(),
                body: body.clone(),
                environment,
            }))))
        }
        Expression::CallExpression {
            function,
            arguments,
        } => {
            let function = eval_expression(function, Rc::clone(&environment))?;

            match &*function {
                Value::Func(func) => {
                    if func.parameters.len() != arguments.len() {
                        Err(EvalError::Error(format!(
                            "wrong number of arguments. wanted {} but got {}",
                            func.parameters.len(),
                            arguments.len()
                        )))?
                    }
                    let arguments = eval_expressions(arguments, environment)?;
                    apply_function(func, arguments)
                }
                other => Err(EvalError::Error(format!("not a function: {}", other))),
            }
        }
        Expression::StringLiteral { value } => Ok(Rc::new(Value::StringValue(value.clone()))),
    }
}

fn eval_expressions(
    expressions: &Vec<Expression>,
    environment: Rc<RefCell<Environment>>,
) -> Result<Vec<Rc<Value>>, EvalError> {
    let mut result = Vec::new();

    for expression in expressions {
        let evaluated = eval_expression(expression, Rc::clone(&environment))?;
        result.push(evaluated);
    }

    Ok(result)
}

fn apply_function(function: &Function, arguments: Vec<Rc<Value>>) -> EvalResult {
    let env = Environment::new_enclosed(Rc::clone(&function.environment));

    for (idx, identifier) in function.parameters.iter().enumerate() {
        env.borrow_mut()
            .set(identifier.clone(), Rc::clone(&arguments[idx]));
    }

    let result = eval_statements(&function.body.statements, env);

    result
}

fn eval_prefix_expression(operator: &PrefixOperator, right: Rc<Value>) -> EvalResult {
    match operator {
        PrefixOperator::BANG => eval_bang_operator_expression(right),
        PrefixOperator::MINUS => eval_minus_operator_expression(right),
    }
}

fn eval_infix_expression(
    operator: &InfixOperator,
    left: Rc<Value>,
    right: Rc<Value>,
) -> EvalResult {
    match (&*left, &*right) {
        (&Value::Num(l), &Value::Num(r)) => match operator {
            InfixOperator::PLUS => Ok(Rc::new(Value::Num(l + r))),
            InfixOperator::MINUS => Ok(Rc::new(Value::Num(l - r))),
            InfixOperator::ASTERISK => Ok(Rc::new(Value::Num(l * r))),
            InfixOperator::SLASH => match r {
                Integer(0) => Err(Error(format!("division by 0: {}/{}", l, r))),
                Float(0.0) => Err(Error(format!("division by 0: {}/{}", l, r))),
                _ => Ok(Rc::new(Value::Num(l / r))),
            },
            InfixOperator::LT => Ok(native_bool_to_boolean_object(l < r)),
            InfixOperator::GT => Ok(native_bool_to_boolean_object(l > r)),
            InfixOperator::EQ => Ok(native_bool_to_boolean_object(l == r)),
            InfixOperator::NOT_EQ => Ok(native_bool_to_boolean_object(l != r)),
        },
        (left, right) if std::mem::discriminant(left) == std::mem::discriminant(right) => {
            match operator {
                InfixOperator::EQ => Ok(native_bool_to_boolean_object(left == right)),
                InfixOperator::NOT_EQ => Ok(native_bool_to_boolean_object(left != right)),
                other => Err(Error(format!(
                    "unknown operator: {} {} {}",
                    left, other, right
                ))),
            }
        }
        (left, right) => Err(Error(format!(
            "type mismatch: {} {} {}",
            left, operator, right
        ))),
    }
}

fn eval_bang_operator_expression(right: Rc<Value>) -> EvalResult {
    match &*right {
        Value::Boolean(true) => Ok(Rc::new(FALSE)),
        Value::Boolean(false) => Ok(Rc::new(TRUE)),
        other => Err(Error(format!("unknown operator: !{}", other))),
    }
}

fn native_bool_to_boolean_object(input: bool) -> Rc<Value> {
    let result = match input {
        true => TRUE,
        false => FALSE,
    };
    Rc::new(result)
}

fn eval_minus_operator_expression(right: Rc<Value>) -> EvalResult {
    match &*right {
        &Value::Num(num) => Ok(Rc::new(Value::Num(-num))),
        other => Err(Error(format!("unknown operator: -{}", other))),
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
            None => assert_eq!(&*evaluated, &Value::Null),
        }
    }
}

#[test]
fn test_return_statements() {
    struct Test<'a>(&'a str, i64);

    let tests = vec![
        Test("return 10;", 10),
        Test("return 10; 9;", 10),
        Test("return 2 * 5; 9;", 10),
        Test("9; return 2 * 5; 9;", 10),
    ];

    for Test(input, expected) in tests {
        let evaluated = test_eval(input).expect("evaluation failed");
        test_integer_object(evaluated, expected)
    }
}

#[test]
fn test_error_handling() {
    struct Test<'a>(&'a str, &'a str);

    let tests = vec![
        Test("5 + true;", "type mismatch: 5 + true"),
        Test("5 + true; 5;", "type mismatch: 5 + true"),
        Test("-true", "unknown operator: -true"),
        Test("true + false;", "unknown operator: true + false"),
        Test("5; true + false; 5", "unknown operator: true + false"),
        Test(
            "if (10 > 1) { true + false; }",
            "unknown operator: true + false",
        ),
        Test(
            r"if (10 > 1) { 
                if (10 > 1) { 
                    return true + false; 
                }
                return 1;
            }",
            "unknown operator: true + false",
        ),
        Test(
            "if (16) { true + false; }",
            "type mismatch: expected bool, but got 16",
        ),
        Test("1 / 0", "division by 0: 1/0"),
        Test("1. / 0.", "division by 0: 1/0"),
        Test("foobar", "identifier not found: foobar"),
    ];

    for Test(input, error_msg) in tests {
        let evaluated = test_eval(input);

        match evaluated {
            Err(Error(error)) => assert_eq!(error, error_msg),
            other => panic!("expected an error but got {:?}", other),
        }
    }
}

#[test]
fn test_let_statements() {
    struct Test<'a>(&'a str, i64);

    let tests = vec![
        Test("let a = 5; a;", 5),
        Test("let a = 5 * 5; a;", 25),
        Test("let a = 5; let b = a; b;", 5),
        Test("let a = 5; let b = a; let c = a + b + 5; c;", 15),
    ];

    for Test(input, expected) in tests {
        let evaluated = test_eval(input).expect(&format!("evaluation failed: {}", input));
        test_integer_object(evaluated, expected)
    }
}

#[test]
fn test_function_literal() {
    let input = "fn(x) { x + 2; };";

    let evaluated = test_eval(input).expect(&format!("evaluation failed: {}", input));
    match &*evaluated {
        Value::Func(func) => {
            let Function {
                parameters, body, ..
            } = &**func;
            assert_eq!(parameters.len(), 1);
            assert_eq!(parameters[0], Identifier("x".to_owned()));
            assert_eq!(body.to_string(), "(x + 2)".to_owned())
        }
        other => panic!("expected a Function but got {:?}", other),
    }
}

#[test]
fn test_function_application() {
    struct Test<'a>(&'a str, i64);

    let tests = vec![
        Test("let identity = fn(x) { x; }; identity(5);", 5),
        Test("let identity = fn(x) { return x; }; identity(5);", 5),
        Test("let double = fn(x) { x * 2; }; double(5);", 10),
        Test("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
        Test("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
        Test("fn(x) { x; }(5)", 5),
    ];

    for Test(input, expected) in tests {
        let evaluated = test_eval(input).expect(&format!("evaluation failed: {}", input));
        test_integer_object(evaluated, expected)
    }
}

#[test]
fn test_closures() {
    let input = "
    let newAdder = fn(x) {
        fn(y) { x + y };
    };

    let addTwo = newAdder(2);
    addTwo(2);
    ";

    let env = Environment::new();

    let evaluated =
        test_eval_with_env(input, Rc::clone(&env)).expect(&format!("evaluation failed: {}", input));

    test_integer_object(evaluated, 4);

    let input = "x";

    test_eval_with_env(input, Rc::clone(&env)).expect_err("closure leaked");
}

#[test]
fn test_string_literal() {
    let input = r#" "Hello World!" "#;

    let evaluated = test_eval(input).expect(&format!("evaluation failed: {}", input));

    match &*evaluated {
        Value::StringValue(s) => assert_eq!(s, "Hello World!"),
        other => panic!("expected StringValue but got {:?}", other),
    }
}

fn test_eval(input: &str) -> EvalResult {
    let lexer = Lexer::new(input);
    let parser = Parser::new(lexer);
    let program = parser.parse_program().map_err(|e| {
        Error(
            e.iter()
                .map(|e| e.to_string())
                .collect::<Vec<String>>()
                .join(", "),
        )
    })?;

    let env = Environment::new();
    eval(Prog(program), env)
}

fn test_eval_with_env(input: &str, environment: Rc<RefCell<Environment>>) -> EvalResult {
    let lexer = Lexer::new(input);
    let parser = Parser::new(lexer);
    let program = parser.parse_program().map_err(|e| {
        Error(
            e.iter()
                .map(|e| e.to_string())
                .collect::<Vec<String>>()
                .join(", "),
        )
    })?;
    eval(Prog(program), environment)
}

fn test_float_object(obj: Rc<Value>, expected: f64) {
    match &*obj {
        &Value::Num(n) => match n {
            Float(i) => assert_eq!(i, expected),
            other => panic!("expected Integer but got {:?}", other),
        },
        other => panic!("expected Integer but got {:?}", other),
    }
}

fn test_integer_object(obj: Rc<Value>, expected: i64) {
    match &*obj {
        &Value::Num(n) => match n {
            Integer(i) => assert_eq!(i, expected),
            other => panic!("expected Integer but got {:?}", other),
        },
        other => panic!("expected Integer but got {:?}", other),
    }
}

fn test_boolean_object(obj: Rc<Value>, expected: bool) {
    match &*obj {
        &Value::Boolean(b) => assert_eq!(b, expected),
        other => panic!("expected Integer but got {:?}", other),
    }
}
