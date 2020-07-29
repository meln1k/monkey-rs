mod ast;
mod environment;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod builtins;

fn main() {
    println!("Hello user! This is the Monkey programming language!");
    println!("Feel free to type in commands");
    repl::start();
}
