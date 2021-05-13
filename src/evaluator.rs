use crate::ast::*;
use crate::builtins;
use crate::environment::Environment;
use crate::evaluator::EvalError::{Error, NonLocalReturnControl};
use crate::object::Numeric::{Float, Integer};
use crate::object::{Function, Hash, Hashable, Numeric, Value, FALSE, TRUE};
use core::fmt;
use std::cell::RefCell;
use std::collections::HashMap;
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

pub type EvalResult = Result<Rc<Value>, EvalError>;

pub fn eval(
    Program { ref statements }: Program,
    environment: Rc<RefCell<Environment>>,
) -> EvalResult {
    eval_statements(statements, environment)
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
        Statement::Return(ReturnStatement { return_value }) => {
            let value = eval_expression(return_value, environment)?;
            Err(NonLocalReturnControl(value))
        }
        Statement::Let(LetStatement { name, value }) => {
            if builtins::builtins(&name.0).is_some() {
                Err(Error(format!("name '{}' is reserved", &name.0)))?
            }
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
            let builtin = builtins::builtins(&name.0);
            let env = (&*environment).borrow().get(&name);
            match builtin.or(env) {
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
                Value::BuiltinFunc(b) => {
                    let arguments = eval_expressions(arguments, environment)?;
                    builtins::apply_builtin_function(b, arguments)
                }
                other => Err(EvalError::Error(format!("not a function: {}", other))),
            }
        }
        Expression::StringLiteral { value } => Ok(Rc::new(Value::StringValue(value.clone()))),
        Expression::ArrayLiteral { elements } => {
            let elements = eval_expressions(elements, environment)?;
            Ok(Rc::new(Value::Array(elements)))
        }
        Expression::IndexExpression { left, index } => {
            let left_eval = eval_expression(left, Rc::clone(&environment))?;
            let index_eval = eval_expression(index, environment)?;

            match (&*left_eval, &*index_eval) {
                (Value::Array(elems), &Value::Num(Numeric::Integer(index))) => {
                    if index < 0 || index > (elems.len()) as i64 - 1 {
                        Ok(Rc::new(Value::Null))
                    } else {
                        let value = &elems[index as usize];
                        Ok(Rc::clone(value))
                    }
                }
                (Value::HashObj(Hash { pairs }), index) => {
                    let key = match index {
                        Value::StringValue(s) => Hashable::String(s.clone()),
                        &Value::Boolean(b) => Hashable::Boolean(b),
                        &Value::Num(Numeric::Integer(i)) => Hashable::Int(i),
                        _ => Err(EvalError::Error(format!("unusable as hash key")))?,
                    };
                    Ok(Rc::clone(pairs.get(&key).unwrap_or(&Rc::new(Value::Null))))
                }
                other => Err(EvalError::Error(format!(
                    "index operator not supported: {}",
                    other.0
                ))),
            }
        }
        Expression::HashLiteral { pairs } => {
            let mut map = HashMap::new();
            for (key_expr, value_expr) in pairs {
                let key = eval_expression(key_expr, Rc::clone(&environment))?;
                let hashable = match &*key {
                    Value::StringValue(s) => Hashable::String(s.clone()),
                    Value::Boolean(b) => Hashable::Boolean(*b),
                    Value::Num(Numeric::Integer(i)) => Hashable::Int(*i),
                    other => Err(EvalError::Error(format!("unusable as hash key: {}", other)))?,
                };
                let value = eval_expression(value_expr, Rc::clone(&environment))?;
                map.insert(hashable, value);
            }
            Ok(Rc::new(Value::HashObj(Hash { pairs: map })))
        }
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
                Float(f) if f == 0f64 => Err(Error(format!("division by 0: {}/{}", l, r))),
                _ => Ok(Rc::new(Value::Num(l / r))),
            },
            InfixOperator::LT => Ok(native_bool_to_boolean_object(l < r)),
            InfixOperator::GT => Ok(native_bool_to_boolean_object(l > r)),
            InfixOperator::EQ => Ok(native_bool_to_boolean_object(l == r)),
            InfixOperator::NOT_EQ => Ok(native_bool_to_boolean_object(l != r)),
        },
        (Value::StringValue(l), Value::StringValue(r)) => match operator {
            InfixOperator::PLUS => Ok(Rc::new(Value::StringValue(format!("{}{}", l, r)))),
            other => Err(Error(format!(
                "unknown operator: {} {} {}",
                left, other, right
            ))),
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

#[cfg(test)]
mod tests {

    use crate::lexer::lexer::Lexer;
    use crate::parser::Parser;
    use super::*;

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
            Test(r#""Hello" - "World""#, "unknown operator: Hello - World"),
            Test(
                r#"{"name": "Monkey"}[fn(x) { x }];"#,
                "unusable as hash key",
            ),
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

        let evaluated = test_eval_with_env(input, Rc::clone(&env))
            .expect(&format!("evaluation failed: {}", input));

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

    #[test]
    fn test_string_concatenation() {
        let input = r#" "Hello" + " " + "World!" "#;

        let evaluated = test_eval(input).expect(&format!("evaluation failed: {}", input));

        match &*evaluated {
            Value::StringValue(s) => assert_eq!(s, "Hello World!"),
            other => panic!("expected StringValue but got {:?}", other),
        }
    }

    #[test]
    fn test_builtin_function() {
        struct Test<'a>(&'a str, ExpectedResult<'a>);

        enum ExpectedResult<'a> {
            Success(i64),
            Error(&'a str),
        }

        use ExpectedResult::*;

        let tests = vec![
            Test(r#"len("")"#, Success(0)),
            Test(r#"len("four")"#, Success(4)),
            Test(r#"len("hello world")"#, Success(11)),
            Test(r#"len(1)"#, Error("argument to 'len' not supported, got 1")),
            Test(
                r#"len("one", "two")"#,
                Error("wrong number of arguments. got=2, want=1"),
            ),
        ];

        for Test(input, expected) in tests {
            match expected {
                Success(int) => {
                    let evaluated = test_eval(input).expect("eval error");
                    test_integer_object(evaluated, int)
                }
                Error(msg) => match test_eval(input) {
                    Err(EvalError::Error(message)) => assert_eq!(msg, message.as_str()),
                    _ => panic!("expected EvalError::Error(message)"),
                },
            }
        }
    }

    #[test]
    fn test_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";

        let evaluated = test_eval(input).expect("eval error");

        match &*evaluated {
            Value::Array(elements) => {
                assert_eq!(elements.len(), 3);
                test_integer_object(Rc::clone(&elements[0]), 1);
                test_integer_object(Rc::clone(&elements[1]), 4);
                test_integer_object(Rc::clone(&elements[2]), 6);
            }
            _ => panic!("expected Array"),
        }
    }

    #[test]
    fn test_array_index_expressions() {
        struct Test(&'static str, Option<i64>);

        let tests = vec![
            Test("[1, 2, 3][0]", Some(1)),
            Test("[1, 2, 3][1]", Some(2)),
            Test("[1, 2, 3][2]", Some(3)),
            Test("let i = 0; [1][i];", Some(1)),
            Test("[1, 2, 3][1 + 1];", Some(3)),
            Test("let myArray = [1, 2, 3]; myArray[2];", Some(3)),
            Test(
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                Some(6),
            ),
            Test(
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                Some(2),
            ),
            Test("[1, 2, 3][3]", None),
            Test("[1, 2, 3][-1]", None),
        ];

        for Test(input, expected) in tests {
            let evaluated = test_eval(input).expect("eval error");
            match expected {
                Some(i) => test_integer_object(evaluated, i),
                None => test_null_object(evaluated),
            }
        }
    }

    #[test]
    fn test_hash_literals() {
        let input = r#"
    let two = "two";
    {
        "one": 10 - 9,
        two: 1 + 1,
        "thr" + "ee": 6 / 2,
        4: 4,
        true: 5,
        false: 6
    }"#;

        let mut expected = HashMap::new();
        expected.insert(Hashable::String("one".to_owned()), 1);
        expected.insert(Hashable::String("two".to_owned()), 2);
        expected.insert(Hashable::String("three".to_owned()), 3);
        expected.insert(Hashable::Int(4), 4);
        expected.insert(Hashable::Boolean(true), 5);
        expected.insert(Hashable::Boolean(false), 6);

        let evaluated = test_eval(input).expect("eval error");

        match &*evaluated {
            Value::HashObj(Hash { pairs }) => {
                assert_eq!(pairs.len(), 6);
                for (expected_key, expected_value) in expected {
                    let pair = pairs
                        .get(&expected_key)
                        .expect("no pair for given key in Pairs");
                    test_integer_object(Rc::clone(pair), expected_value)
                }
            }
            _ => panic!("expected Array"),
        }
    }

    #[test]
    fn test_hash_index_expressions() {
        struct Test(&'static str, Option<i64>);

        let tests = vec![
            Test(r#"{"foo": 5}["foo"]"#, Some(5)),
            Test(r#"{"foo": 5}["bar"]"#, None),
            Test(r#"let key = "foo"; {"foo": 5}[key]"#, Some(5)),
            Test(r#"{}["foo"]"#, None),
            Test(r#"{5: 5}[5]"#, Some(5)),
            Test(r#"{true: 5}[true]"#, Some(5)),
            Test(r#"{false: 5}[false]"#, Some(5)),
        ];

        for Test(input, expected) in tests {
            let evaluated = test_eval(input).expect("eval error");
            match expected {
                Some(i) => test_integer_object(evaluated, i),
                None => test_null_object(evaluated),
            }
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
        eval(program, env)
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
        eval(program, environment)
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

    fn test_null_object(obj: Rc<Value>) {
        match &*obj {
            Value::Null => (),
            other => panic!("expected Null but got {:?}", other),
        }
    }
}
