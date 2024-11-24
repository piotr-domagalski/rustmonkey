#![allow(unused)]

use crate::ast::{TokenIter, Statement, ParsingError};
use std::iter::Peekable;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program {
    pub statements: Vec<Statement>
}
impl Program {
    pub fn new_from_statements(statements: &[Statement]) -> Program{
        Program {
            statements: statements.to_vec()
        }

    }
}
impl Program {
    pub fn parse<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Program, ParsingError> 
    {
        let mut statements: Vec<Statement> = vec![];
        let mut errors: Vec<&str> = vec![];
    
        loop {
            match Statement::parse(iter) {
                Ok(statement) => statements.push(statement),
                Err("EOF") => break,
                Err(error) => errors.push(error),
            }
        }
    
        if errors.is_empty() {
            Ok(Program {statements})
        }
        else {
            Err(errors.into())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::Token;
    use crate::lexer::Lexer;

    #[test]
    fn test_token_iter_trait() {
        let tokens = vec![Token::Let, Token::Identifier(String::from("x")), Token::Assign, Token::Integer(5), Token::Semicolon];
        let parsed_from_tokens = Program::parse(&mut tokens.into_iter().peekable());
        
        let code = "let x = 5;";
        let lexer = Lexer::new(code);
        let parsed_from_lexer = Program::parse(&mut lexer.peekable());

        assert_eq!(parsed_from_tokens, parsed_from_lexer);
    }
}
