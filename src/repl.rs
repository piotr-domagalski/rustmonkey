use text_io::read;
use crate::lexer::Lexer;

pub fn start() {
    loop {
        print!(">>> ");
        let input: String = read!("{}\n");
        dbg!(Lexer::new(&input).collect::<Vec<_>>());
    }
} 
