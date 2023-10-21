use std::cell::RefCell;
use std::rc::Rc;

use crate::environment::Environment;
use crate::lexer::lexer::Lexer;
use crate::parser::Parser;
use crate::object::Value;
use crate::evaluator;

#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wasm")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[cfg_attr(feature = "wasm", wasm_bindgen)]
pub struct Repl {
    buffer: String,
    environment: Rc<RefCell<Environment>>,
    in_multiline_statement: bool,
}

#[cfg_attr(feature = "wasm", wasm_bindgen)]
impl Repl {
    #[cfg_attr(feature = "wasm", wasm_bindgen(constructor))]
    pub fn new() -> Repl {
        Repl { buffer: String::new(), environment: Environment::new(), in_multiline_statement: false }
    }

    pub fn can_show_prompt(&self) -> bool {
        !self.in_multiline_statement
    }

    pub fn eval(&mut self, incoming_line: &str) -> Option<String> {
        self.buffer.push_str(incoming_line);

        if is_even_parenthesis(&self.buffer).is_err() {
            self.in_multiline_statement = false;
            return Some("evaluation error: wrong order of braces".to_owned())
        }

        if !is_even_parenthesis(&self.buffer).unwrap() {
            self.in_multiline_statement = true;
            return None
        }

        self.in_multiline_statement = false;
        let lexer = Lexer::new(&self.buffer);
        let parser = Parser::new(lexer);

        let output = match parser.parse_program() {
            Ok(program) => match evaluator::eval(program, Rc::clone(&self.environment)) {
                Ok(obj) => match *obj {
                    Value::Null => None,
                    _ => Some(format!("{}", obj))                    
                }
                Err(err) => Some(format!("evaluation error: {}", err)),
            },
            Err(errs) => {
                let mut errors_str = String::new();
                for err in errs {
                    errors_str.push_str(&format!("{}", err));
                }
                Some(errors_str)
            }
        };

        if !self.in_multiline_statement {
            self.buffer.clear();
        }

        return output

    }
}

fn is_even_parenthesis(input: &str) -> Result<bool, ()> {
    let mut parens = Vec::new();

    for char in input.chars() {
        match char {
            '(' => parens.push('('),
            ')' => {
                if parens.pop() != Some('(') {
                    Err(())?
                }
            }
            '{' => parens.push('{'),
            '}' => {
                if parens.pop() != Some('{') {
                    Err(())?
                }
            }
            _ => (),
        }
    }

    Ok(parens.is_empty())
}
