use crate::object::{BuiltinFunction, Value};
use std::rc::Rc;
use crate::object::Value::BuiltinFunc;

pub fn builtins(name: &str) -> Option<Rc<Value>> {
    match name {
        "len" => Some(Rc::new(BuiltinFunc(BuiltinFunction::Len))),
        _ => None
    }
}