use std::iter::Peekable;
use crate::token::Token;
use crate::ast::{
    TokenIter,
    Expression,
    IdentifierExpression,
    ParsingError,
    Statement,
    BlockStatement,
};

//parsing
impl Statement {
    pub fn parse<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Statement, ParsingError>
    {
        match iter.peek() {
            None => Err(ParsingError::new_other("EOF")),
            Some(Token::Let) => Self::parse_let_statement(iter),
            Some(Token::Return) => Self::parse_return_statement(iter),
            Some(Token::LeftCurly) => Self::parse_block_statement(iter),
            _ => Self::parse_expression_statement(iter),
        }
    }

    fn parse_let_statement<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Statement, ParsingError> {
        next_if_eq_else_return_err!(iter, Token::Let, "let statement", unexpected);
        let identifier = IdentifierExpression::parse(iter)?;
        next_if_eq_else_return_err!(iter, Token::Assign, "let statement", unexpected);
        let expression = Expression::parse(iter)?;
        next_if_eq_else_return_err!(iter, Token::Semicolon, "let statement", unexpected);

        Ok(Statement::new_let(identifier, expression))
    }

    fn parse_return_statement<I: TokenIter> (iter: &mut Peekable<I>) -> Result<Statement, ParsingError>
    {
        next_if_eq_else_return_err!(iter, Token::Return, "return statement", unexpected);
        let expression = Expression::parse(iter)?;
        next_if_eq_else_return_err!(iter, Token::Semicolon, "return statement", unexpected);

        Ok(Statement::new_return(expression))
    }

    fn parse_expression_statement<I: TokenIter> (iter: &mut Peekable<I>) -> Result<Statement, ParsingError> {
        let expression = Expression::parse(iter)?;
        iter.next_if_eq(&Token::Semicolon);

        Ok(Statement::new_expr(expression))
    }

    fn parse_block_statement<I: TokenIter> (iter : &mut Peekable<I>) -> Result<Statement, ParsingError> {
        Ok(Statement::new_block(BlockStatement::parse(iter)?))
    }
}

impl BlockStatement {
    pub fn parse<I: TokenIter>(iter: &mut Peekable<I>) -> Result<BlockStatement, ParsingError> {
        next_if_eq_else_return_err!(iter, Token::LeftCurly, "block statement", unexpected);
        let mut statements = vec![];
        let mut errors = vec![];
        loop {
            match iter.peek() {
                Some(&Token::RightCurly) => { iter.next(); break; },
                None => { return Err(ParsingError::new_other("unclosed curly brace")); },
                _ => {
                    match Statement::parse(iter) {
                        Ok(statement) => { statements.push(statement); },
                        Err(err) => { errors.push(err); },
                    }
                }
            }
        }

        if errors.is_empty() {
            return Ok(BlockStatement::new(statements));
        } else {
            return Err(ParsingError::new_multiple(errors));
        }
    }
}
