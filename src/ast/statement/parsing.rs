use std::iter::Peekable;
use crate::ast::{
    Expression,
    IdentifierExpression,
    Statement,
    BlockStatement,
};
use crate::parsing::{
    Parsable,
    Token,
    TokenIter,
    ParsingError,
    next_if_eq_else_return_err,
};

use super::parsing_what_consts::*;

impl Parsable for Statement {
    fn parse<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Statement, ParsingError>
    {
        match iter.peek() {
            None => Err(ParsingError::new_unexpected(None, vec![], PARSING_WHAT_STMT)),
            Some(Token::Let) => Self::parse_let_statement(iter),
            Some(Token::Return) => Self::parse_return_statement(iter),
            Some(Token::LeftCurly) => Self::parse_block_statement(iter),
            _ => Self::parse_expression_statement(iter), //TODO: list tokens
        }
    }
}

//parsing helpers
impl Statement {
    fn parse_let_statement<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Statement, ParsingError> {
        next_if_eq_else_return_err!(iter, Token::Let, PARSING_WHAT_LET_STMT, unexpected);
        let identifier = IdentifierExpression::parse(iter)?;
        next_if_eq_else_return_err!(iter, Token::Assign, PARSING_WHAT_LET_STMT, unexpected);
        let expression = Expression::parse(iter)?;
        next_if_eq_else_return_err!(iter, Token::Semicolon, PARSING_WHAT_LET_STMT, unexpected);

        Ok(Statement::new_let(identifier, expression))
    }

    fn parse_return_statement<I: TokenIter> (iter: &mut Peekable<I>) -> Result<Statement, ParsingError>
    {
        next_if_eq_else_return_err!(iter, Token::Return, PARSING_WHAT_RETURN_STMT, unexpected);
        let expression = Expression::parse(iter)?;
        next_if_eq_else_return_err!(iter, Token::Semicolon, PARSING_WHAT_RETURN_STMT, unexpected);

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

impl Parsable for BlockStatement {
    fn parse<I: TokenIter>(iter: &mut Peekable<I>) -> Result<BlockStatement, ParsingError> {
        next_if_eq_else_return_err!(iter, Token::LeftCurly, PARSING_WHAT_BLOCK_STMT, unexpected);
        let mut statements = vec![];
        let mut errors = vec![];
        loop {
            match iter.peek() {
                Some(&Token::RightCurly) => { iter.next(); break; },
                None => { return Err(ParsingError::new_other("unclosed curly brace", PARSING_WHAT_BLOCK_STMT)); },
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
            return Err(ParsingError::new_multiple(errors, PARSING_WHAT_BLOCK_STMT));
        }
    }
}
