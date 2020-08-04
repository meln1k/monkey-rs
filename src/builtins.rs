use crate::evaluator::EvalError;
use crate::evaluator::EvalResult;
use crate::object::Value::BuiltinFunc;
use crate::object::{Numeric, Value};
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub enum BuiltinFunction {
    Len,
    First,
    Last,
    Rest,
    Push,
    Puts,
}

pub fn builtins(name: &str) -> Option<Rc<Value>> {
    match name {
        "len" => Some(Rc::new(BuiltinFunc(BuiltinFunction::Len))),
        "first" => Some(Rc::new(BuiltinFunc(BuiltinFunction::First))),
        "last" => Some(Rc::new(BuiltinFunc(BuiltinFunction::Last))),
        "rest" => Some(Rc::new(BuiltinFunc(BuiltinFunction::Rest))),
        "push" => Some(Rc::new(BuiltinFunc(BuiltinFunction::Push))),
        "puts" => Some(Rc::new(BuiltinFunc(BuiltinFunction::Puts))),
        _ => None,
    }
}

pub fn apply_builtin_function(builin: &BuiltinFunction, arguments: Vec<Rc<Value>>) -> EvalResult {
    match builin {
        BuiltinFunction::Len => {
            if arguments.len() != 1 {
                wrong_nr_of_args_msg(arguments.len(), 1)?;
            }
            match &*arguments[0] {
                Value::StringValue(string) => {
                    Ok(Rc::new(Value::Num(Numeric::Integer(string.len() as i64))))
                }
                Value::Array(elems) => {
                    Ok(Rc::new(Value::Num(Numeric::Integer(elems.len() as i64))))
                }
                other => wrong_arg_error_msg(builin, other),
            }
        }
        BuiltinFunction::First => {
            if arguments.len() != 1 {
                wrong_nr_of_args_msg(arguments.len(), 1)?;
            }
            match &*arguments[0] {
                Value::Array(elems) => {
                    Ok(Rc::clone(elems.first().unwrap_or(&Rc::new(Value::Null))))
                }
                other => wrong_arg_error_msg(builin, other),
            }
        }
        BuiltinFunction::Last => {
            if arguments.len() != 1 {
                wrong_nr_of_args_msg(arguments.len(), 1)?;
            }
            match &*arguments[0] {
                Value::Array(elems) => Ok(Rc::clone(elems.last().unwrap_or(&Rc::new(Value::Null)))),
                other => wrong_arg_error_msg(builin, other),
            }
        }
        BuiltinFunction::Rest => {
            if arguments.len() != 1 {
                wrong_nr_of_args_msg(arguments.len(), 1)?;
            }
            match &*arguments[0] {
                Value::Array(elems) => {
                    let result = elems
                        .get(1..elems.len())
                        .map_or(Value::Null, |s| Value::Array(s.to_vec()));
                    Ok(Rc::new(result))
                }
                other => wrong_arg_error_msg(builin, other),
            }
        }
        BuiltinFunction::Push => {
            if arguments.len() != 2 {
                wrong_nr_of_args_msg(arguments.len(), 2)?;
            }
            match &*arguments[0] {
                Value::Array(elems) => {
                    let mut copied = elems.clone();
                    copied.push(Rc::clone(&arguments[1]));
                    Ok(Rc::new(Value::Array(copied)))
                }
                other => Err(EvalError::Error(format!(
                    "argument to 'push' must be ARRAY, got {}",
                    other
                ))),
            }
        }
        BuiltinFunction::Puts => {
            for arg in arguments {
                println!("{}", arg);
            }
            Ok(Rc::new(Value::Null))
        }
    }
}

fn wrong_arg_error_msg(func: &BuiltinFunction, other: &Value) -> EvalResult {
    Err(EvalError::Error(format!(
        "argument to '{}' not supported, got {}",
        func, other
    )))
}

fn wrong_nr_of_args_msg(number: usize, wanted: usize) -> EvalResult {
    Err(EvalError::Error(format!(
        "wrong number of arguments. got={}, want={}",
        number, wanted
    )))
}
