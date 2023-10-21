use std::io::{self, Write};

use interpreter::repl::Repl;

fn main() {
    println!("Hello user! This is the Monkey programming language version {}!", env!("CARGO_PKG_VERSION"));
    println!("Feel free to type in commands");
    start();
}

static PROMT: &str = ">> ";

pub fn start() {
    let stdin = io::stdin();

    let mut repl = Repl::new();

    loop {
        if repl.can_show_prompt() {
            print!("{}", PROMT);
            io::stdout().flush().unwrap();
        }

        let mut buffer = String::new();
        match stdin.read_line(&mut buffer) {
            Ok(_) => match repl.eval(&buffer) {
                Some(output) => println!("{}", output),
                None => continue,
            },
            Err(err) => {
                println!("{:?}", err);
                return;
            }
        }
    }
}
