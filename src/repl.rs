use text_io::read;
use crate::parsing::Lexer;
use crate::ast::Statement;

pub fn start() {
    loop {
        print!(">>> ");
        let input: String = read!("{}\n");
        dbg!(Lexer::new(&input).collect::<Vec<_>>());
        let lexer =  Lexer::new(&input);
        let result = Statement::parse(&mut lexer.peekable());
        match result {
            Ok(Statement::Expression { expression }) => { println!("{};", expression); },
            Ok(statement) => { dbg!(statement); },
            Err(err) => { println!("{}", err); },
        }
    }
}
