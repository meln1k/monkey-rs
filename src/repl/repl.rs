use crate::lexer::lexer::Lexer;
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
                for token in lexer {
                    println!("{:?}", token)
                }
            }
            Err(err) => {
                println!("{:?}", err);
                return;
            }
        }

        buffer.clear();
    }
}
