use core::fmt;
use std::fmt::{Display, Formatter};
use std::ops::{Add, Div, Mul, Neg, Sub};

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Object {
    Num(Numeric),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
}

#[derive(Debug, PartialEq, PartialOrd)]
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

pub const TRUE: Object = Object::Boolean(true);
pub const FALSE: Object = Object::Boolean(false);

use crate::object::Object::*;

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Num(n) => write!(f, "{}", n),
            Boolean(b) => write!(f, "{}", b),
            Null => write!(f, "null"),
            ReturnValue(obj) => write!(f, "{}", *obj),
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
