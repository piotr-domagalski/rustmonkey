mod token;
mod lexer;
mod repl;

fn main() {
    println!("Welcome to the Monkey interpreter.");
    println!("Type some code to see it tokenised.");
    repl::start();
}
