use crate::ast::{TokenIter, Statement, ParsingError};
use std::iter::Peekable;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program {
    pub statements: Vec<Statement>
}
impl Program {
    #[allow(dead_code, reason = "this might be needed for testing")]
    pub fn new_from_statements(statements: &[Statement]) -> Program{
        Program {
            statements: statements.to_vec()
        }

    }
}

impl Program {
    #[allow(dead_code, reason = "this will be used once source file interpretation is implemented")]
    pub fn parse<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Program, ParsingError>
    {
        let mut statements: Vec<Statement> = vec![];
        let mut errors: Vec<ParsingError> = vec![];

        loop {
            match Statement::parse(iter) {
                Ok(statement) => statements.push(statement),
                Err(ParsingError::OtherError { message }) if message == "EOF" => break,
                Err(error) => errors.push(error),
            }
        }

        if errors.is_empty() {
            Ok(Program {statements})
        }
        else {
            Err(ParsingError::new_multiple(errors))
        }
    }
}
