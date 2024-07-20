#![allow(unused)]

use crate::token::Token;
use crate::ast::TokenIter;
use std::iter::Peekable;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum PrefixOperator {
    Inverse,
    Negation,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    Identifier { identifier_expression: IdentifierExpression},
    Literal { literal: Literal},
    Prefix { operator: PrefixOperator, expression: Box<Expression> },
    /*
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Call(CallExpression),
    If(IfExpression),
    */
}
//builders
impl Expression {
    pub fn new_ident(identifier: &str) -> Expression{
        Expression::Identifier { identifier_expression: IdentifierExpression::new(identifier) }
    }
    pub fn new_int(integer: i64) -> Expression {
        Expression::Literal { literal: Literal::new_int(integer) }
    }
    pub fn new_bool(boolean: bool) -> Expression {
        Expression::Literal { literal: Literal::new_bool(boolean) }
    }
    pub fn new_prefix(operator: PrefixOperator, expression: Expression) -> Expression {
        Expression::Prefix { operator, expression: Box::new(expression) }
    }
    //TODO new_infix
}
//parsing
impl Expression {
    pub fn parse<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Expression, &'static str>
    {
        let left_expression = match iter.peek() {
            Some(Token::Identifier(_)) => Ok(Expression::Identifier {identifier_expression: IdentifierExpression::parse(iter)? }),
            Some(Token::Integer(_)) => Ok(Expression::Literal {literal: Literal::parse(iter)? }),
            Some(Token::Bang) | Some(Token::Minus) => Expression::parse_prefix_expression(iter),
            _ => return Err("unimplemented expression type")
        };

        //TEMP
        left_expression
    }

    fn parse_prefix_expression<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Expression, &'static str> {
        let operator = match iter.peek() {
            Some(Token::Minus) => {
                iter.next();
                PrefixOperator::Inverse
            },
            Some(Token::Bang) => {
                iter.next();
                PrefixOperator::Negation
            },
            _ => return Err("Expected ! or - operator"),
        };
        Ok(Expression::Prefix { operator: operator, expression: Box::new(Expression::parse(iter)?)})
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IdentifierExpression{
    pub identifier: String,
}

impl IdentifierExpression {
    pub fn new(identifier: &str) -> IdentifierExpression {
        IdentifierExpression{ identifier: String::from(identifier) }
    }
}

impl IdentifierExpression {
    fn parse<I: TokenIter>(iter: &mut Peekable<I>) -> Result<IdentifierExpression, &'static str> {
        match iter.peek() {
            Some(Token::Identifier(ident)) => {
                let ident = ident.clone();
                iter.next();
                Ok(IdentifierExpression { identifier: ident })
            },
            _ => Err("expected identifier token"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Literal{
    Integer(i64),
    Bool(bool),
}
impl Literal {
    pub fn new_int(integer: i64) -> Literal {
        Literal::Integer(integer)
    }
    pub fn new_bool(boolean: bool) -> Literal {
        Literal::Bool(boolean)
    }
}
impl Literal {
    fn parse<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Literal, &'static str> {
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
            _ => 
                Err("expected integer literal token"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::Token;
    #[test]
    fn test_identifier_expression() {
        let input = [
            Token::Identifier(String::from("foobar")),
            Token::Semicolon,
        ];
        let expected = Expression::new_ident("foobar");

        let mut iterator = input.into_iter().peekable();
        let parsed = Expression::parse(&mut iterator).expect("Hardcoded tokens shouldn't fail to parse");

        assert_eq!(iterator.next(), Some(Token::Semicolon));
        assert_eq!(parsed, expected);
    }

    #[test]
    fn test_integer_expression() {
        let input = [
            Token::Integer(5),
            Token::Semicolon,
        ];
        let expected = Expression::new_int(5);

        let mut iterator = input.into_iter().peekable();
        let parsed = Expression::parse(&mut iterator).expect("Hardcoded tokens shouldn't fail to parse");

        assert_eq!(iterator.next(), Some(Token::Semicolon));
        assert_eq!(parsed, expected);
    }

    #[test]
    fn test_prefix_expressions() {
        struct Test {
            tokens: [Token; 3],
            expected: Expression
        };
        let tests = [
            Test {
                tokens: [Token::Minus, Token::Integer(5), Token::Semicolon],
                expected: Expression::new_prefix(PrefixOperator::Inverse, Expression::new_int(5)),
            },
            Test {
                tokens: [Token::Bang, Token::Integer(15), Token::Semicolon],
                expected: Expression::new_prefix(PrefixOperator::Negation, Expression::new_int(15)),
            },
        ];

        for test in tests {
            let mut iterator = test.tokens.clone().into_iter().peekable();
            let parsed = Expression::parse(&mut iterator).expect("Hardcoded tokens shouldn't fail to parse");

            assert_eq!(iterator.next(), Some(Token::Semicolon), "input: {:?}, parsed: {:?}", test.tokens, parsed);
            assert_eq!(parsed, test.expected);
        }
    }
}
