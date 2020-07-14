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

    loop {
        println!("{}", PROMT);

        match stdin.read_line(&mut buffer) {
            Ok(_) => {
                let lexer = Lexer::new(&buffer);
                let parser = Parser::new(lexer);

                match parser.parse_program() {
                    Ok(program) => match evaluator::eval(Prog(program), Rc::clone(&environment)) {
                        Ok(obj) => println!("{}", obj),
                        Err(err) => println!("evaluation error {}", err),
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

        buffer.clear();
    }
}
