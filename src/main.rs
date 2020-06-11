mod lexer;
mod repl;

fn main() {
    println!("Hello user! This is the Monkey programming language!");
    println!("Feel free to type in commands");
    repl::repl::start();

}


