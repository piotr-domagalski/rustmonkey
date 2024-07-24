#![allow(unused)]
use crate::token::Token;
use crate::ast::TokenIter;
use crate::ast::Expression;
use crate::ast::IdentifierExpression;
use std::iter::Peekable;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    Let{ identifier: IdentifierExpression, expression: Expression},
    Return { expression: Expression},
    Expression {expression: Expression},
    /*
    IfElse(IfElseStatement),
    Empty,
    None,
    */
}
//builders
impl Statement {
    pub fn new_let(identifier: IdentifierExpression, expression: Expression) -> Statement{
        Statement::Let{
            identifier,
            expression,
        }
    }
    pub fn new_return(expression: Expression) -> Statement {
        Statement::Return {
            expression
        }
    }

    pub fn new_expr(expression: Expression) -> Statement {
        Statement::Expression {
            expression
        }
    }
}
//parsing
impl Statement {
    pub fn parse<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Statement, &'static str>
    {
        match iter.peek() {
            None => Err("EOF"),
            Some(Token::Let) => Self::parse_let_statement(iter),
            Some(Token::Return) => Self::parse_return_statement(iter),
            _ => Self::parse_expression_statement(iter),
            // TODO: Should this be a wildcard? should there be an Err("unimplemented statement type")?
        }
    }

    // let <identifier> = <expression> ;
    fn parse_let_statement<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Statement, &'static str> {
        if iter.next_if_eq(&Token::Let).is_none() { return Err("let keyword expected"); };
        let identifier = IdentifierExpression::parse(iter)?;
        if iter.next_if_eq(&Token::Assign).is_none() { return Err("assignment operator expected"); };
        let expression = Expression::parse(iter)?;
        if iter.next_if_eq(&Token::Semicolon).is_none() { return Err("semicolon expected")};
    
        Ok(Statement::new_let(identifier, expression))
    }

    fn parse_return_statement<I: TokenIter> (iter: &mut Peekable<I>) -> Result<Statement, &'static str>
    {
        if iter.next_if_eq(&Token::Return).is_none() { return Err("return keyword expected"); };
        let expression = Expression::parse(iter)?;
        if iter.next_if_eq(&Token::Semicolon).is_none() { return Err("semicolon expected")};

        Ok(Statement::new_return(expression))
    }

    fn parse_expression_statement<I: TokenIter> (iter: &mut Peekable<I>) -> Result<Statement, &'static str> {
        let expression = Expression::parse(iter)?;
        iter.next_if(|tok| *tok == Token::Semicolon);

        Ok(Statement::new_expr(expression))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::Token;

    #[test]
    fn test_let_statements() {
        struct Input {
            tokens: [Token; 5],
            expected: Statement,
        }
        let inputs = [
            Input {
                tokens: [
                    Token::Let,
                    Token::Identifier(String::from("x")),
                    Token::Assign,
                    Token::Integer(5),
                    Token::Semicolon,
                ],
                expected: Statement::new_let(IdentifierExpression::new("x"), Expression::new_int(5)),
            },

            Input {
                tokens: [
                    Token::Let,
                    Token::Identifier(String::from("y")),
                    Token::Assign,
                    Token::Integer(10),
                    Token::Semicolon,
                ],
                expected: Statement::new_let(IdentifierExpression::new("y"), Expression::new_int(10)),
            },

            Input {
                tokens: [
                    Token::Let,
                    Token::Identifier(String::from("foobar")),
                    Token::Assign,
                    Token::Integer(838383),
                    Token::Semicolon,
                ],
                expected: Statement::new_let(IdentifierExpression::new("foobar"), Expression::new_int(838383))
            },
        ];

        for input in inputs {
            let mut iterator = input.tokens.into_iter().peekable();
            let parsed = Statement::parse(&mut iterator).expect("Hardcoded tokens shouldn't fail to parse");

            assert_eq!(iterator.next(), None);
            assert_eq!(parsed, input.expected);
        }
    }

    #[test]
    fn test_return_statements() {
        struct Input {
            tokens: [Token; 3],
            expected: Statement,
        }
        let inputs = [
            Input {
                tokens: [
                    Token::Return,
                    Token::Integer(5),
                    Token::Semicolon,
                ],
                expected: Statement::new_return(Expression::new_int(5)),
            },

            Input {
                tokens: [
                    Token::Return,
                    Token::Integer(10),
                    Token::Semicolon,
                ],
                expected: Statement::new_return(Expression::new_int(10)),
            },

            Input {
                tokens: [
                    Token::Return,
                    Token::Integer(993322),
                    Token::Semicolon,
                ],
                expected: Statement::new_return(Expression::new_int(993322)),
            }
        ];

        for input in inputs {
            let mut iterator = input.tokens.into_iter().peekable();
            let parsed = Statement::parse(&mut iterator).expect("Hardcoded tokens shouldn't fail to parse");

            assert_eq!(iterator.next(), None);
            assert_eq!(parsed, input.expected);
        }
    }
}
