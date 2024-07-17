mod token;
mod lexer;
mod repl;
mod ast;
mod parser;

fn main() {
    println!("Welcome to the Monkey interpreter.");
    println!("Type some code to see it tokenised.");
    repl::start();
}
