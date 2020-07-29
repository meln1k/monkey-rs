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
            Ok(_) if !is_even_parenthesis(buffer.as_str()) => in_multiline_statement = true,
            Ok(_) => {
                in_multiline_statement = false;
                let lexer = Lexer::new(&buffer);
                let parser = Parser::new(lexer);

                match parser.parse_program() {
                    Ok(program) => match evaluator::eval(Prog(program), Rc::clone(&environment)) {
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

fn is_even_parenthesis(input: &str) -> bool {
    let mut opened_parens = 0;
    let mut opened_braces = 0;

    for char in input.chars() {
        match char {
            '(' => opened_parens += 1,
            ')' => opened_parens -= 1,
            '{' => opened_braces += 1,
            '}' => opened_braces -= 1,
            _ => ()
        }
    }

    opened_parens == 0 && opened_braces == 0
}
