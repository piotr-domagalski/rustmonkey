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
        iter.next();

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
        if let Some(Token::Identifier(ident)) = iter.peek() {
            Ok(IdentifierExpression { identifier: ident.clone() })
        } else {
            Err("expected identifier token")
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
            Some(Token::Integer(i)) => 
                Ok(Literal::Integer(*i)),
            Some(Token::Bool(b)) =>
                Ok(Literal::Bool(*b)),
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

        let output = Expression::parse(&mut input.into_iter().peekable()).expect("hardcoded tokens shouldn't fail to parse");

        if let Expression::Identifier{identifier_expression: IdentifierExpression{identifier}} = output {
            assert_eq!(identifier, "foobar");
        } else {
            panic!("expected Expression::Identifier, got {:?}", output);
        }
    }

    #[test]
    fn test_integer_expression() {
        let input = [
            Token::Integer(5),
            Token::Semicolon,
        ];

        let output = Expression::parse(&mut input.into_iter().peekable()).expect("hardcoded tokens shouldn't fail to parse");

        if let Expression::Literal{literal} = output {
            if let Literal::Integer(int) = literal {
                assert_eq!(int, 5);
            } else {
                panic!("expected integer literal, got {:?}", literal)
            }
        } else {
            panic!("expected Expression::Literal, got {:?}", output);
        }
    }

    #[test]
    fn test_prefix_expressions() {
        struct Test {
            tokens: [Token; 3],
            operator: PrefixOperator,
            integer: i64,
        };
        let tests = [
            Test {
                tokens: [Token::Minus, Token::Integer(5), Token::Semicolon],
                operator: PrefixOperator::Inverse,
                integer: 5,
            },
            Test {
                tokens: [Token::Bang, Token::Integer(15), Token::Semicolon],
                operator: PrefixOperator::Negation,
                integer: 15
            },
        ];

        for test in tests {
            let parsed = Expression::parse(&mut test.tokens.into_iter().peekable()).expect("Hardcoded tokens shouldn't fail to parse");

            if let Expression::Prefix{operator, expression} = parsed {
                assert_eq!(operator, test.operator);
                if let Expression::Literal{literal: Literal::Integer(int)} = *expression {
                    assert_eq!(int, test.integer);
                } else {
                    panic!("expected Expression::Literal(Literal::Integer), got, {:?}", expression)
                }
            } else {
                panic!("expected Expression::Prefix, got {:?}", parsed);
            }
        }
    }
}
