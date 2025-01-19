mod repl;
mod ast;
mod parsing;

fn main() {
    println!("Welcome to the Monkey interpreter.");
    println!("Type some code to see it tokenised and parsed.");
    repl::start();
}
