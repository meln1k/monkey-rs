use std::fmt::{Display, Formatter};
use core::fmt;

#[derive(Debug)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
}

use crate::object::Object::*;
impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Integer(int) => write!(f, "{}", int),
            Boolean(b) => write!(f, "{}", b),
            Null => write!(f, "null"),
        }
    }
}