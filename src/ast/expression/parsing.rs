use crate::ast::{
    TokenIter,
    BlockStatement,
    ParsingError,
    IdentifierExpression,
    Expression,
    Literal,
    InfixOperator,
    PrefixOperator,
    Precedence
};
use crate::token::Token;
use std::iter::Peekable;

use super::parsing_error_consts::*;

//parsing
impl Expression {
    pub fn parse<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Expression, ParsingError> {
        let expr = Self::parse_with_precedence(iter, Precedence::Lowest)?;
        match iter.peek() {
            Some(Token::Semicolon | Token::RightCurly | Token::Comma) | None => Ok(expr),
            Some(Token::RightRound) => Err(ParsingError::new_other("missing opening parenthesis", PARSING_WHAT_EXPR)),
            _ => panic!("parse_with_precedence should never leave the lexer at a token other than ;, ), or None")
        }
    }

    pub fn parse_with_precedence<I: TokenIter>(iter: &mut Peekable<I>, precedence: Precedence) -> Result<Expression, ParsingError> {
        //TODO: macro rule to automatically generate this match and error given token lists and fns
        let mut left = match iter.peek() {
            Some(Token::Identifier(_)) => Expression::parse_identifier_expression(iter)?,
            Some(Token::Integer(_)) | Some(Token::Bool(_)) | Some(Token::Function) => Expression::parse_literal_expression(iter)?,
            Some(Token::Bang) | Some(Token::Minus) => Expression::parse_prefix_expression(iter)?,
            Some(Token::LeftRound) => Expression::parse_grouped_expression(iter)?,
            Some(Token::If) => Expression::parse_if_expression(iter)?,
            other => return Err(ParsingError::new_unexpected(
                            other,
                            vec![Token::AnIdentifier, Token::AnInteger, Token::ABool, Token::Function, Token::Bang, Token::Minus],
                            PARSING_WHAT_EXPR))
        };

        loop {
            match iter.peek() {
                Some(Token::Semicolon | Token::RightRound | Token::RightCurly | Token::Comma ) | None => return Ok(left),
                Some(Token::LeftRound) => {
                    left = Self::parse_call_expression(iter, left)?;
                }
                Some(_) => {
                    let next_precedence = InfixOperator::parse(iter)?.precedence();
                    if precedence >= next_precedence {
                        return Ok(left);
                    }
                    left = Self::parse_infix_expression(iter, left)?;
                },
            }
        }
    }

    fn parse_identifier_expression<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Expression, ParsingError> {
        Ok(Expression::Identifier {identifier_expression: IdentifierExpression::parse(iter)? })
    }

    fn parse_literal_expression<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Expression, ParsingError> {
        Ok(Expression::Literal {literal: Literal::parse(iter)? })
    }

    fn parse_prefix_expression<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Expression, ParsingError> {
        let operator = PrefixOperator::parse(iter)?;
        iter.next(); // operator parsing doesn't consume the token - do it manually
        Ok(Expression::new_prefix(operator, Expression::parse_with_precedence(iter, Precedence::Prefix)?))
    }

    fn parse_infix_expression<I: TokenIter>(iter: &mut Peekable<I>, left: Expression) -> Result<Expression, ParsingError> {
        let operator = InfixOperator::parse(iter)?;
        iter.next(); // operator parsing doesn't consume the token - do it manually
        let right = Self::parse_with_precedence(iter, operator.precedence())?;
        Ok(Expression::Infix{operator, left: Box::new(left), right: Box::new(right)})
    }

    fn parse_grouped_expression<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Expression, ParsingError> {
        next_if_eq_else_return_err!(iter, Token::LeftRound, PARSING_WHAT_GROUP_EXPR, other, "missing opening parenthesis" );
        let out = Expression::parse_with_precedence(iter, Precedence::Lowest)?;
        next_if_eq_else_return_err!(iter, Token::RightRound, PARSING_WHAT_GROUP_EXPR, other, "unclosed parenthesis" );
        return Ok(out);
    }

    fn parse_if_expression<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Expression, ParsingError> {

        next_if_eq_else_return_err!(iter, Token::If, PARSING_WHAT_IF_EXPR, unexpected);
        peek_if_eq_else_return_err!(iter, Token::LeftRound, PARSING_WHAT_IF_EXPR, unexpected);

        let condition = Expression::parse_grouped_expression(iter)?;
        let consequence = BlockStatement::parse(iter)?;
        let alternative = match iter.peek() {
            Some(Token::Else) => {
                iter.next();
                Some(BlockStatement::parse(iter)?)
            },
            _ => None,
        };

        return Ok(Expression::new_if(condition, consequence, alternative));
    }

    fn parse_call_expression<I: TokenIter>(iter: &mut Peekable<I>, left: Expression) -> Result<Expression, ParsingError> {
        next_if_eq_else_return_err!(iter, Token::LeftRound, PARSING_WHAT_CALL_EXPR, unexpected);

        let mut args = vec![];
        loop {
            match iter.peek() {
                Some(&Token::RightRound) => { iter.next(); break; },
                None => { return Err(ParsingError::new_unexpected(iter.peek(), vec![Token::AnIdentifier], PARSING_WHAT_CALL_EXPR)); },
                Some(_) => {
                    args.push(Expression::parse(iter)?);
                    match iter.peek() {
                        Some(Token::Comma) => { iter.next(); },
                        Some(Token::RightRound) => {},
                        _ => { return Err(ParsingError::new_unexpected(iter.peek(), vec![Token::RightRound, Token::Comma], PARSING_WHAT_CALL_EXPR)); },
                    }
                }
            }
        }
        Ok(Expression::new_call(left, args))
    }
}

impl IdentifierExpression {
    pub fn parse<I: TokenIter>(iter: &mut Peekable<I>) -> Result<IdentifierExpression, ParsingError> {
        match iter.peek() {
            Some(Token::Identifier(ident)) => {
                let ident = ident.clone();
                iter.next();
                Ok(IdentifierExpression { identifier: ident })
            },
            _ => Err(ParsingError::new_unexpected(iter.peek(), vec![Token::AnIdentifier], PARSING_WHAT_IDENT_EXPR)),
        }
    }
}

impl Literal {
    pub fn parse<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Literal, ParsingError> {
        match iter.peek() {
            Some(Token::Integer(i)) => {
                let i = *i;
                iter.next();
                Ok(Literal::Integer(i))
            },
            Some(Token::Bool(b)) => {
                let b = *b;
                iter.next();
                Ok(Literal::Bool(b))
            }
            Some(Token::Function) => {
                Ok(Literal::parse_fn_literal(iter)?)
            }
            _ =>
                Err(ParsingError::new_unexpected(
                    iter.peek(),
                    vec![Token::AnInteger, Token::ABool, Token::Function],
                    PARSING_WHAT_LIT_EXPR)),
        }
    }

    fn parse_fn_literal<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Literal, ParsingError> {
        next_if_eq_else_return_err!(iter, Token::Function, PARSING_WHAT_FN_LIT, unexpected);
        next_if_eq_else_return_err!(iter, Token::LeftRound, PARSING_WHAT_FN_LIT, unexpected);
        let mut parameters = vec![];
        let mut expect_end_of_params = true;
        loop {
            match iter.peek() {
                Some(&Token::RightRound) => {
                    let tok = iter.next();
                    if expect_end_of_params { break; }
                    else { return Err(ParsingError::new_unexpected(tok.as_ref(), vec![Token::AnIdentifier], PARSING_WHAT_FN_LIT)); }
                },

                Some(&Token::Identifier(_)) => {
                    parameters.push(IdentifierExpression::parse(iter)?);
                    match iter.peek() {
                        Some(Token::Comma) => { expect_end_of_params = false; iter.next(); },
                        Some(Token::RightRound) => { expect_end_of_params = true; },
                        _ => { return Err(ParsingError::new_unexpected(iter.peek(), vec![Token::RightRound, Token::Comma], PARSING_WHAT_FN_LIT)); },
                    }
                }
                _ => { return Err(ParsingError::new_unexpected(iter.peek(), vec![Token::AnIdentifier], PARSING_WHAT_FN_LIT)); },
            }
        }
        let body = BlockStatement::parse(iter)?;
        Ok(Literal::new_fn(parameters, body))
    }
}
