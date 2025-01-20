mod repl;
mod ast;
mod parsing;
mod testing_common;

fn main() {
    println!("Welcome to the Monkey interpreter.");
    println!("Type some code to see it tokenised and parsed.");
    repl::start();
}
