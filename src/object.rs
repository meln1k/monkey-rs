use core::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Add, Div, Mul, Neg, Sub};

#[derive(Debug, PartialEq)]
pub enum Value {
    Num(Numeric),
    Boolean(bool),
    Null,
    Func(Rc<Function>),
    StringValue(String),
}
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub environment: Rc<RefCell<Environment>>,
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self as *const Function == other as *const Function
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Function({}) {{...}}",
            self.parameters
                .iter()
                .map(|i| i.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(Debug, PartialEq, PartialOrd, Copy, Clone)]
pub enum Numeric {
    Integer(i64),
    Float(f64),
}

impl Add for Numeric {
    type Output = Numeric;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Numeric::Float(l), Numeric::Float(r)) => Numeric::Float(l + r),
            (Numeric::Float(l), Numeric::Integer(r)) => Numeric::Float(l + r as f64),
            (Numeric::Integer(l), Numeric::Float(r)) => Numeric::Float(l as f64 + r),
            (Numeric::Integer(l), Numeric::Integer(r)) => Numeric::Integer(l + r),
        }
    }
}

impl Sub for Numeric {
    type Output = Numeric;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Numeric::Float(l), Numeric::Float(r)) => Numeric::Float(l - r),
            (Numeric::Float(l), Numeric::Integer(r)) => Numeric::Float(l - r as f64),
            (Numeric::Integer(l), Numeric::Float(r)) => Numeric::Float(l as f64 - r),
            (Numeric::Integer(l), Numeric::Integer(r)) => Numeric::Integer(l - r),
        }
    }
}

impl Mul for Numeric {
    type Output = Numeric;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Numeric::Float(l), Numeric::Float(r)) => Numeric::Float(l * r),
            (Numeric::Float(l), Numeric::Integer(r)) => Numeric::Float(l * r as f64),
            (Numeric::Integer(l), Numeric::Float(r)) => Numeric::Float(l as f64 * r),
            (Numeric::Integer(l), Numeric::Integer(r)) => Numeric::Integer(l * r),
        }
    }
}

impl Div for Numeric {
    type Output = Numeric;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Numeric::Float(l), Numeric::Float(r)) => Numeric::Float(l / r),
            (Numeric::Float(l), Numeric::Integer(r)) => Numeric::Float(l / r as f64),
            (Numeric::Integer(l), Numeric::Float(r)) => Numeric::Float(l as f64 / r),
            (Numeric::Integer(l), Numeric::Integer(r)) => Numeric::Integer(l / r),
        }
    }
}

impl Neg for Numeric {
    type Output = Numeric;

    fn neg(self) -> Self::Output {
        match self {
            Numeric::Float(n) => Numeric::Float(-n),
            Numeric::Integer(n) => Numeric::Integer(-n),
        }
    }
}

pub const TRUE: Value = Value::Boolean(true);
pub const FALSE: Value = Value::Boolean(false);

use crate::ast::{BlockStatement, Identifier};
use crate::environment::Environment;
use crate::object::Value::*;
use std::cell::RefCell;
use std::rc::Rc;

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Num(n) => write!(f, "{}", n),
            Boolean(b) => write!(f, "{}", b),
            Null => write!(f, "null"),
            Func(func) => write!(
                f,
                "fn({}){{\n{}\n}}",
                func.parameters
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                func.body.to_string()
            ),
            StringValue(s) => write!(f, "{}", s),
        }
    }
}

impl Display for Numeric {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Numeric::Float(n) => write!(f, "{}", n),
            Numeric::Integer(n) => write!(f, "{}", n),
        }
    }
}
