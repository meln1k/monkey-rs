use crate::ast::Node::Prog;
use crate::evaluator;
use crate::lexer::lexer::Lexer;
use crate::parser::Parser;
use std::io;

static PROMT: &str = ">> ";

pub fn start() {
    let mut buffer = String::new();

    let stdin = io::stdin();

    loop {
        println!("{}", PROMT);

        match stdin.read_line(&mut buffer) {
            Ok(_) => {
                let lexer = Lexer::new(&buffer);
                let parser = Parser::new(lexer);
                match parser.parse_program() {
                    Ok(program) => match evaluator::eval(Prog(program)) {
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
