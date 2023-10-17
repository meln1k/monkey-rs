use crate::environment::Environment;
use crate::evaluator;
use crate::lexer::lexer::Lexer;
use crate::parser::Parser;
use std::cell::RefCell;
use std::io;
use std::rc::Rc;

static PROMT: &str = ">> ";

pub struct Repl {
    buffer: String,
    environment: Rc<RefCell<Environment>>,
    in_multiline_statement: bool,
}


impl Repl {
    pub fn new() -> Repl {
        Repl { buffer: String::new(), environment: Environment::new(), in_multiline_statement: false }
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
                Ok(obj) => format!("{}", obj),
                Err(err) => format!("evaluation error: {}", err),
            },
            Err(errs) => {
                let mut errors_str = String::new();
                for err in errs {
                    errors_str.push_str(&format!("{}", err));
                }
                errors_str
            }
        };

        if !self.in_multiline_statement {
            self.buffer.clear();
        }

        return Some(output)

    }
}


pub fn start() {

    let stdin = io::stdin();

    let mut repl = Repl::new();

    loop {

        if !&repl.in_multiline_statement {
            println!("{}", PROMT);
        }

        let mut buffer = String::new();
        match stdin.read_line(&mut buffer) {
            Ok(_) => {
                match repl.eval(&buffer) {
                    Some(output) => println!("{}", output),
                    None => continue
                }
            }
            Err(err) => {
                println!("{:?}", err);
                return;
            }
        }
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
