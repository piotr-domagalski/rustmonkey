#![cfg(test)]
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
            ],
            expected: Err(ParsingError::new_unexpected(None, vec![Token::Semicolon], "let statement")),
            next_tok: None,
        },
    ];

    for Input {tokens, expected, next_tok} in inputs {
        let mut iterator = tokens.clone().into_iter().peekable();
        let parsed = Statement::parse(&mut iterator);

        assert_eq!(parsed, expected, "{:?}", tokens);
        assert_eq!(iterator.next(), next_tok, "{:?}", tokens);
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
        let mut iterator = tokens.clone().into_iter().peekable();
        let parsed = Statement::parse(&mut iterator);

        assert_eq!(parsed, expected, "{:?}", tokens);
        assert_eq!(iterator.next(), next_tok);
    }
}

use crate::ast::expression::{IdentifierExpression, InfixOperator};

#[test]
fn test_block_statement() {
    struct Test {
        tokens: Vec<Token>,
        expected: Result<Statement, ParsingError>,
        next_tok: Option<Token>,
    }

    let tests = vec![
        Test {
            tokens: vec![
                Token::LeftCurly,
                Token::Let, Token::Identifier("x".to_string()), Token::Assign, Token::Integer(5), Token::Semicolon,
                Token::Identifier("x".to_string()), Token::LessThan, Token::Integer(3), Token::Semicolon,
                Token::RightCurly,
            ],
            expected: Ok(Statement::new_block(
                BlockStatement::new(vec![
                    Statement::new_let(IdentifierExpression::new("x"), Expression::new_int(5)),
                    Statement::new_expr(Expression::new_infix(InfixOperator::LessThan, Expression::new_ident("x"), Expression::new_int(3))),
                ]))),
            next_tok: None,
        },
        Test {
            tokens: vec![
                Token::LeftCurly,
                Token::Identifier("x".to_string()), Token::LessThan, Token::Integer(3), Token::Semicolon,
                Token::RightCurly,
            ],
            expected: Ok(Statement::new_block(
                BlockStatement::new(vec![
                    Statement::new_expr(Expression::new_infix(InfixOperator::LessThan, Expression::new_ident("x"), Expression::new_int(3))),
                ]))),
            next_tok: None,
        },
        Test {
            tokens: vec![
                Token::LeftCurly,
                Token::RightCurly,
            ],
            expected: Ok(Statement::new_block(BlockStatement::new(vec![]))),
            next_tok: None,
        },
        Test {
            tokens: vec![
                Token::LeftCurly,
                Token::Let, Token::Identifier("x".to_string()), Token::Assign, Token::Integer(5), Token::Semicolon,
                Token::Identifier("x".to_string()), Token::LessThan, Token::Integer(3), Token::Semicolon,
            ],
            expected: Err(ParsingError::new_other("unclosed curly brace")),
            next_tok: None,
        },
    ];

    for Test {tokens, expected, next_tok} in tests {
        let mut iterator = tokens.clone().into_iter().peekable();
        let parsed = Statement::parse(&mut iterator);

        assert_eq!(parsed, expected, "{:?}", tokens);
        assert_eq!(iterator.next(), next_tok, "{:?}", tokens);
    }
}
