#![allow(unused)]
use crate::token::Token;
use crate::ast::TokenIter;
use crate::ast::Expression;
use crate::ast::IdentifierExpression;
use crate::ast::ParsingError;
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
    pub fn parse<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Statement, ParsingError>
    {
        match iter.peek() {
            None => Err(ParsingError::new_other("EOF")),
            Some(Token::Let) => Self::parse_let_statement(iter),
            Some(Token::Return) => Self::parse_return_statement(iter),
            _ => Self::parse_expression_statement(iter),
        }
    }

    // let <identifier> = <expression> ;
    fn parse_let_statement<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Statement, ParsingError> {
        if iter.next_if_eq(&Token::Let).is_none() { return Err(ParsingError::new_unexpected(iter.peek(), vec![Token::Let], "let statement")); }
        let identifier = IdentifierExpression::parse(iter)?;
        if iter.next_if_eq(&Token::Assign).is_none() { return Err(ParsingError::new_unexpected(iter.peek(), vec![Token::Assign], "let statement")); }
        let expression = Expression::parse(iter)?;
        if iter.next_if_eq(&Token::Semicolon).is_none() { return Err(ParsingError::new_unexpected(iter.peek(), vec![Token::Semicolon], "let statement")); }
    
        Ok(Statement::new_let(identifier, expression))
    }

    // return <expression> ;
    fn parse_return_statement<I: TokenIter> (iter: &mut Peekable<I>) -> Result<Statement, ParsingError>
    {
        if iter.next_if_eq(&Token::Return).is_none() { return Err(ParsingError::new_unexpected(iter.peek(), vec![Token::Return], "return statement")); }
        let expression = Expression::parse(iter)?;
        if iter.next_if_eq(&Token::Semicolon).is_none() { return Err(ParsingError::new_unexpected(iter.peek(), vec![Token::Semicolon], "return statement")); };

        Ok(Statement::new_return(expression))
    }

    fn parse_expression_statement<I: TokenIter> (iter: &mut Peekable<I>) -> Result<Statement, ParsingError> {
        let expression = Expression::parse(iter)?;
        iter.next_if_eq(&Token::Semicolon);

        Ok(Statement::new_expr(expression))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::Token;

    #[test]
    fn test_statements() {
        struct Input {
            tokens: Vec<Token>,
            expected: Result<Statement, ParsingError>,
            next_tok: Option<Token>
        }
        let inputs = [
            Input {
                tokens: vec![],
                expected: Err(ParsingError::new_other("EOF")),
                next_tok: None,
            },

            Input {
                tokens: vec![
                    Token::Let,
                    Token::Identifier(String::from("x")),
                    Token::Assign,
                    Token::Integer(5),
                    Token::Semicolon,
                ],
                expected: Ok(Statement::new_let(IdentifierExpression::new("x"), Expression::new_int(5))),
                next_tok: None,
            },

            Input {
                tokens: vec![
                    Token::Return,
                    Token::Integer(5),
                    Token::Semicolon,
                ],
                expected: Ok(Statement::new_return(Expression::new_int(5))),
                next_tok: None,
            },
        ];

        for Input { tokens, expected, next_tok } in inputs {
            let mut iterator = tokens.into_iter().peekable();
            let parsed = Statement::parse(&mut iterator);

            assert_eq!(parsed, expected);
            assert_eq!(iterator.next(), next_tok);
        }
    }

    #[test]
    fn test_let_statements() {
        struct Input {
            tokens: Vec<Token>,
            expected: Result<Statement, ParsingError>,
            next_tok: Option<Token>,
        }
        let inputs = [
            Input {
                tokens: vec![
                    Token::Let,
                    Token::Identifier(String::from("x")),
                    Token::Assign,
                    Token::Integer(5),
                    Token::Semicolon,
                ],
                expected: Ok(Statement::new_let(IdentifierExpression::new("x"), Expression::new_int(5))),
                next_tok: None,
            },

            Input {
                tokens: vec![
                    Token::Let,
                    Token::Identifier(String::from("y")),
                    Token::Assign,
                    Token::Integer(10),
                    Token::Semicolon,
                ],
                expected: Ok(Statement::new_let(IdentifierExpression::new("y"), Expression::new_int(10))),
                next_tok: None,
            },

            Input {
                tokens: vec![
                    Token::Let,
                    Token::Identifier(String::from("foobar")),
                    Token::Assign,
                    Token::Integer(838383),
                    Token::Semicolon,
                ],
                expected: Ok(Statement::new_let(IdentifierExpression::new("foobar"), Expression::new_int(838383))),
                next_tok: None,
            },

            //missing let
            Input {
                tokens: vec![
                    Token::Return,
                    Token::Identifier(String::from("x")),
                ],
                expected: Err(ParsingError::new_unexpected(Some(&Token::Return), vec![Token::Let], "let statement")),
                next_tok: Some(Token::Return),
            },

            //missing assign
            Input {
                tokens: vec![
                    Token::Let,
                    Token::Identifier(String::from("x")),
                    Token::Bang,
                    Token::Integer(838383),
                    Token::Semicolon
                ],
                expected: Err(ParsingError::new_unexpected(Some(&Token::Bang), vec![Token::Assign], "let statement")),
                next_tok: Some(Token::Bang),
            },

            //missing semicolon
            Input {
                tokens: vec![
                    Token::Let,
                    Token::Identifier(String::from("x")),
                    Token::Assign,
                    Token::Integer(838383),
                    Token::Bang
                ],
                expected: Err(ParsingError::new_unexpected(Some(&Token::Bang), vec![Token::Semicolon], "let statement")),
                next_tok: Some(Token::Bang),
            },
        ];

        for Input {tokens, expected, next_tok} in inputs {
            let mut iterator = tokens.into_iter().peekable();
            let parsed = Statement::parse_let_statement(&mut iterator);

            assert_eq!(parsed, expected);
            assert_eq!(iterator.next(), next_tok);
        }
    }

    #[test]
    fn test_return_statements() {
        struct Input {
            tokens: Vec<Token>,
            expected: Result<Statement, ParsingError>,
            next_tok: Option<Token>,
        }
        let inputs = [
            Input {
                tokens: vec![
                    Token::Return,
                    Token::Integer(5),
                    Token::Semicolon,
                ],
                expected: Ok(Statement::new_return(Expression::new_int(5))),
                next_tok: None,
            },

            Input {
                tokens: vec![
                    Token::Return,
                    Token::Integer(10),
                    Token::Semicolon,
                ],
                expected: Ok(Statement::new_return(Expression::new_int(10))),
                next_tok: None,
            },

            Input {
                tokens: vec![
                    Token::Return,
                    Token::Integer(993322),
                    Token::Semicolon,
                ],
                expected: Ok(Statement::new_return(Expression::new_int(993322))),
                next_tok: None,
            },

            //missing return
            Input {
                tokens: vec![
                    Token::Let,
                    Token::Integer(993322),
                ],
                expected: Err(ParsingError::new_unexpected(Some(&Token::Let), vec![Token::Return], "return statement")),
                next_tok: Some(Token::Let),
            },

            //missing semicolon
            Input {
                tokens: vec![
                    Token::Return,
                    Token::Integer(993322),
                ],
                expected: Err(ParsingError::new_unexpected(None, vec![Token::Semicolon], "return statement")),
                next_tok: None,
            },
        ];

        for Input {tokens, expected, next_tok} in inputs {
            let mut iterator = tokens.into_iter().peekable();
            let parsed = Statement::parse_return_statement(&mut iterator);

            assert_eq!(parsed, expected);
            assert_eq!(iterator.next(), next_tok);
        }
    }

}
