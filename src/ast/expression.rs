#![allow(unused)]

use crate::token::Token;
use crate::ast::TokenIter;
use std::iter::Peekable;

#[derive(Debug)]
pub enum Expression {
    Identifier { identifier_expression: IdentifierExpression},
    Literal { literal: Literal},
    Prefix { prefix_expression: Box<PrefixExpression> },
    /*
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Call(CallExpression),
    If(IfExpression),
    */
}
impl Expression {
    pub fn parse<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Expression, &'static str>
    {
        let left_expression = match iter.peek() {
            Some(Token::Identifier(_)) => Ok(Expression::Identifier {identifier_expression: IdentifierExpression::parse(iter)? }),
            Some(Token::Integer(_)) => Ok(Expression::Literal {literal: Literal::parse(iter)? }),
            Some(Token::Bang) | Some(Token::Minus) => Ok(Expression::Prefix {prefix_expression: Box::new(PrefixExpression::parse(iter)?) }),
            _ => return Err("unimplemented expression type")
        };

        //TEMP
        iter.next();

        left_expression
    }
}

#[derive(Debug)]
pub struct IdentifierExpression{
    pub identifier: String,
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

#[derive(Debug)]
pub enum Literal{
    Integer(i64),
    Bool(bool),
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
#[derive(Debug)]
pub enum PrefixExpression{
    Inverse{expression: Expression},
    Negation{expression: Expression},
}
impl PrefixExpression {
    fn parse<I: TokenIter>(iter: &mut Peekable<I>) -> Result<PrefixExpression, &'static str> {
        match iter.peek() {
            Some(Token::Minus) => {
                iter.next();
                Ok(PrefixExpression::Inverse{expression: Expression::parse(iter)?})
            }
            Some(Token::Bang) => {
                iter.next();
                Ok(PrefixExpression::Negation{expression: Expression::parse(iter)?})
            }
            _ => Err("Expected ! or -"),
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
    fn test_prefix_inverse() {
        let input = [Token::Minus, Token::Integer(5), Token::Semicolon];

        let output = Expression::parse(&mut input.into_iter().peekable()).expect("hardcoded tokens shouldn't fail to parse");
    
        if let Expression::Prefix{prefix_expression} = output {
            if let PrefixExpression::Inverse{expression} = *prefix_expression {
                if let Expression::Literal{literal: Literal::Integer(int)} = expression{
                    assert_eq!(int, 5);
                } else {
                    panic!("expected Expression::Literal(Literal::Integer), got, {:?}", expression)
                }
            } else {
                panic!("expected PrefixExpression::Inverse, got {:?}", *prefix_expression);
            }
        } else {
            panic!("expected Expression::Prefix, got {:?}", output);
        }
    }

    #[test]
    fn test_prefix_negation() {
        let input = [Token::Bang, Token::Integer(15), Token::Semicolon];

        let output = Expression::parse(&mut input.into_iter().peekable()).expect("hardcoded tokens shouldn't fail to parse");
    
        if let Expression::Prefix{prefix_expression} = output {
            if let PrefixExpression::Negation{expression} = *prefix_expression {
                if let Expression::Literal{literal: Literal::Integer(int)} = expression{
                    assert_eq!(int, 15);
                } else {
                    panic!("expected Expression::Literal(Literal::Integer), got, {:?}", expression)
                }
            } else {
                panic!("expected PrefixExpression::Inverse, got {:?}", *prefix_expression);
            }
        } else {
            panic!("expected Expression::Prefix, got {:?}", output);
        }
    }
}
