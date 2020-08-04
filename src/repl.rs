use crate::ast::Node::Prog;
use crate::environment::Environment;
use crate::evaluator;
use crate::lexer::lexer::Lexer;
use crate::parser::Parser;
use std::io;
use std::rc::Rc;

static PROMT: &str = ">> ";

pub fn start() {
    let mut buffer = String::new();

    let stdin = io::stdin();

    let environment = Environment::new();

    let mut in_multiline_statement = false;

    loop {
        if !in_multiline_statement {
            println!("{}", PROMT);
        }

        match stdin.read_line(&mut buffer) {
            Ok(_) if is_even_parenthesis(buffer.as_str()).is_err() => {
                in_multiline_statement = false;
                println!("evaluation error: wrong order of braces")
            }
            Ok(_) if !is_even_parenthesis(buffer.as_str()).unwrap() => {
                in_multiline_statement = true
            }
            Ok(_) => {
                in_multiline_statement = false;
                let lexer = Lexer::new(&buffer);
                let parser = Parser::new(lexer);

                match parser.parse_program() {
                    Ok(program) => match evaluator::eval(program, Rc::clone(&environment)) {
                        Ok(obj) => println!("{}", obj),
                        Err(err) => println!("evaluation error: {}", err),
                    },
                    Err(errs) => {
                        for err in errs {
                            println!("{}", err)
                        }
                    }
                };
            }
            Err(err) => {
                println!("{:?}", err);
                return;
            }
        }

        if !in_multiline_statement {
            buffer.clear();
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
