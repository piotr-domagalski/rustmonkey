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

//parsing
impl Expression {
    pub fn parse<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Expression, ParsingError> {
        let expr = Self::parse_with_precedence(iter, Precedence::Lowest)?;
        match iter.peek() {
            Some(Token::Semicolon | Token::RightCurly | Token::Comma) | None => Ok(expr),
            Some(Token::RightRound) => Err(ParsingError::new_other("missing opening parenthesis")),
            _ => panic!("parse_with_precedence should never leave the lexer at a token other than ;, ), or None")
        }
    }

    pub fn parse_with_precedence<I: TokenIter>(iter: &mut Peekable<I>, precedence: Precedence) -> Result<Expression, ParsingError> {
        let mut left = match iter.peek() {
            Some(Token::Identifier(_)) => Expression::Identifier {identifier_expression: IdentifierExpression::parse(iter)? },
            Some(Token::Integer(_)) | Some(Token::Bool(_)) | Some(Token::Function) => Expression::Literal {literal: Literal::parse(iter)? },
            Some(Token::Bang) | Some(Token::Minus) => Expression::parse_prefix_expression(iter)?,
            Some(Token::LeftRound) => Expression::parse_grouped_expression(iter)?,
            Some(Token::If) => Expression::parse_if_expression(iter)?,
            _ => return Err(ParsingError::new_unexpected(
                            iter.peek(),
                            vec![Token::Identifier("".to_string()), Token::Integer(0), Token::Bool(true), Token::Function, Token::Bang, Token::Minus],
                            "expression"))
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
        if iter.next_if_eq(&Token::LeftRound).is_none() { return Err(ParsingError::new_other("missing opening parenthesis")); };
        let out = Expression::parse_with_precedence(iter, Precedence::Lowest)?;
        if iter.next_if_eq(&Token::RightRound).is_none() { return Err(ParsingError::new_other("unclosed parenthesis")); };
        return Ok(out);
    }

    fn parse_if_expression<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Expression, ParsingError> {
        match iter.peek() {
            Some(&Token::If) => { iter.next(); },
            other => { return Err(ParsingError::new_unexpected(other, vec![Token::If], "if expression")); },
        }
        match iter.peek() {
            Some(&Token::LeftRound) => {},
            other => { return Err(ParsingError::new_unexpected(other, vec![Token::LeftRound], "if expression")); },
        }

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
        match iter.peek() {
            Some(&Token::LeftRound) => { iter.next(); },
            other => { return Err(ParsingError::new_unexpected(other, vec![Token::LeftRound], "call expression")); },
        }

        let mut args = vec![];
        loop {
            match iter.peek() {
                Some(&Token::RightRound) => { iter.next(); break; },
                None => { return Err(ParsingError::new_unexpected(iter.peek(), vec![Token::Identifier("".to_string())], "call expression")); },
                Some(_) => {
                    args.push(Expression::parse(iter)?);
                    match iter.peek() {
                        Some(Token::Comma) => { iter.next(); },
                        Some(Token::RightRound) => {},
                        _ => { return Err(ParsingError::new_unexpected(iter.peek(), vec![Token::RightRound, Token::Comma], "call expression")); },
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
            //TODO: fix this once there are proper token markers
            _ => Err(ParsingError::new_unexpected(iter.peek(), vec![Token::Identifier("".to_string())], "identifier expression")),
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
                    //TODO: Fix this once these tokens store Option<_>
                    vec![Token::Integer(0), Token::Bool(true), Token::Function],
                    "literal expression")),
        }
    }

    fn parse_fn_literal<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Literal, ParsingError> {
        if iter.next_if_eq(&Token::Function).is_none() {
            return Err(ParsingError::new_unexpected(iter.peek(), vec![Token::Function], "function literal"));
        }
        if iter.next_if_eq(&Token::LeftRound).is_none() {
            return Err(ParsingError::new_unexpected(iter.peek(), vec![Token::LeftRound], "function literal"));
        }
        let mut parameters = vec![];
        loop {
            match iter.peek() {
                Some(&Token::RightRound) => { iter.next(); break; },
                Some(&Token::Identifier(_)) => {
                    parameters.push(IdentifierExpression::parse(iter)?);
                    match iter.peek() {
                        Some(Token::Comma) => { iter.next(); },
                        Some(Token::RightRound) => {},
                        _ => { return Err(ParsingError::new_unexpected(iter.peek(), vec![Token::RightRound, Token::Comma], "function literal")); },
                    }
                }
                _ => { return Err(ParsingError::new_unexpected(iter.peek(), vec![Token::Identifier("".to_string())], "function literal")); },
            }
        }
        let body = BlockStatement::parse(iter)?;
        Ok(Literal::new_fn(parameters, body))
    }
}
