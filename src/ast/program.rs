use crate::token::Token;
use crate::ast::TokenIter;
use crate::ast::Statement;
use std::iter::Peekable;

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>
}
impl Program {
    pub fn parse_program<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Program, Vec<&'static str>> 
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
            Err(errors)
        }
    }
}
