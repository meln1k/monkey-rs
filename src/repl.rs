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
                    Ok(program) => println!("{}", program),
                    Err(err) => println!("Parsing error: {}", err.join(", ")),
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
