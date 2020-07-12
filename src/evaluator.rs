use crate::ast::Node::Prog;
use crate::ast::Statement::Block;
use crate::ast::*;
use crate::environment::Environment;
use crate::evaluator::EvalError::{Error, NonLocalReturnControl};
use crate::lexer::lexer::Lexer;
use crate::object::Numeric::{Float, Integer};
use crate::object::{Object, FALSE, TRUE};
use crate::parser::Parser;
use core::fmt;
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum EvalError {
    NonLocalReturnControl(Object),
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

type EvalResult = Result<Object, EvalError>;

pub struct Evaluator {
    environment: Environment,
}

impl Evaluator {
    pub fn new(environment: Environment) -> Evaluator {
        Evaluator { environment }
    }

    pub fn eval(&mut self, node: Node) -> EvalResult {
        match node {
            Node::Prog(Program { statements }) => self.eval_statements(statements),
            other => todo!(),
        }
    }

    fn eval_statements(&mut self, statements: Vec<Statement>) -> EvalResult {
        let mut obj: Option<Object> = None;

        for statement in statements {
            match self.eval_statement(statement) {
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

        Ok(obj.unwrap_or(Object::Null))
    }

    fn eval_statement(&mut self, statement: Statement) -> EvalResult {
        match statement {
            Statement::Expr(ExpressionStatement { expression }) => self.eval_expression(expression),
            Statement::Block(BlockStatement { statements }) => self.eval_statements(statements),
            Statement::Return(ReturnStatement { return_value }) => {
                let value = self.eval_expression(return_value)?;
                Err(NonLocalReturnControl(value))
            }
            Statement::Let(LetStatement { name, value }) => {
                let eval_value = self.eval_expression(value)?;
                Ok(self.environment.set(name, eval_value))
            }
        }
    }

    fn eval_expression(&mut self, expression: Expression) -> EvalResult {
        match expression {
            Expression::IntegerLiteral(int) => Ok(Object::Num(Integer(int))),
            Expression::FloatLiteral(float) => Ok(Object::Num(Float(float))),
            Expression::Boolean(value) => Ok(self.native_bool_to_boolean_object(value)),
            Expression::PrefixExpression { operator, expr } => {
                let right = self.eval_expression(*expr)?;
                self.eval_prefix_expression(operator, right)
            }
            Expression::InfixExpression {
                left,
                operator,
                right,
            } => {
                let left = self.eval_expression(*left)?;
                let right = self.eval_expression(*right)?;
                self.eval_infix_expression(operator, left, right)
            }
            Expression::IfExpression {
                condition,
                consequence,
                alternative,
            } => {
                let cond = self.eval_expression(*condition)?;
                match cond {
                    Object::Boolean(true) => self.eval_statement(Block(*consequence)),
                    Object::Boolean(false) => match alternative {
                        Some(block) => self.eval_statement(Block(*block)),
                        None => Ok(Object::Null),
                    },
                    other => Err(Error(format!(
                        "type mismatch: expected bool, but got {}",
                        other
                    ))),
                }
            }
            Expression::Identifier(name) => match self.environment.get(&name) {
                Some(&obj) => Ok(obj),
                None => Err(Error(format!("identifier not found: {}", name))),
            },
            _ => todo!(),
        }
    }

    fn eval_prefix_expression(&self, operator: PrefixOperator, right: Object) -> EvalResult {
        match operator {
            PrefixOperator::BANG => self.eval_bang_operator_expression(right),
            PrefixOperator::MINUS => self.eval_minus_operator_expression(right),
        }
    }

    fn eval_infix_expression(
        &self,
        operator: InfixOperator,
        left: Object,
        right: Object,
    ) -> EvalResult {
        match (left, right) {
            (Object::Num(l), Object::Num(r)) => match operator {
                InfixOperator::PLUS => Ok(Object::Num(l + r)),
                InfixOperator::MINUS => Ok(Object::Num(l - r)),
                InfixOperator::ASTERISK => Ok(Object::Num(l * r)),
                InfixOperator::SLASH => match r {
                    Integer(0) => Err(Error(format!("division by 0: {}/{}", l, r))),
                    Float(0.0) => Err(Error(format!("division by 0: {}/{}", l, r))),
                    _ => Ok(Object::Num(l / r)),
                },
                InfixOperator::LT => Ok(self.native_bool_to_boolean_object(l < r)),
                InfixOperator::GT => Ok(self.native_bool_to_boolean_object(l > r)),
                InfixOperator::EQ => Ok(self.native_bool_to_boolean_object(l == r)),
                InfixOperator::NOT_EQ => Ok(self.native_bool_to_boolean_object(l != r)),
            },
            (left, right) if std::mem::discriminant(&left) == std::mem::discriminant(&right) => {
                match operator {
                    InfixOperator::EQ => Ok(self.native_bool_to_boolean_object(left == right)),
                    InfixOperator::NOT_EQ => Ok(self.native_bool_to_boolean_object(left != right)),
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

    fn eval_bang_operator_expression(&self, right: Object) -> EvalResult {
        match right {
            Object::Boolean(true) => Ok(FALSE),
            Object::Boolean(false) => Ok(TRUE),
            other => Err(Error(format!("unknown operator: !{}", other))),
        }
    }

    fn native_bool_to_boolean_object(&self, input: bool) -> Object {
        match input {
            true => TRUE,
            false => FALSE,
        }
    }

    fn eval_minus_operator_expression(&self, right: Object) -> EvalResult {
        match right {
            Object::Num(int) => Ok(Object::Num(-int)),
            other => Err(Error(format!("unknown operator: -{}", other))),
        }
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
        let evaluated = test_eval(input).expect("evaluation failed");
        test_integer_object(evaluated, expected)
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
    let mut evaluator = Evaluator::new(env);
    evaluator.eval(Prog(program))
}

fn test_float_object(obj: Object, expected: f64) {
    match obj {
        Object::Num(n) => match n {
            Float(i) => assert_eq!(i, expected),
            other => panic!("expected Integer but got {:?}", other),
        },
        other => panic!("expected Integer but got {:?}", other),
    }
}

fn test_integer_object(obj: Object, expected: i64) {
    match obj {
        Object::Num(n) => match n {
            Integer(i) => assert_eq!(i, expected),
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
