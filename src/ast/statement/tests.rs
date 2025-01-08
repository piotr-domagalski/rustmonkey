#![cfg(test)]
use super::*;
use super::parsing_what_consts::*;

use crate::token::Token;
use crate::ast::ParsingError;
use crate::ast::expression::{IdentifierExpression, InfixOperator};

use crate::testing_common::{ParsingTest, test_parser};

#[test]
fn test_statements() {
    type Test = ParsingTest<Statement>;

    let tests = [
        Test {
            test_name: "error_eof",
            tokens: vec![],
            expected: Err(ParsingError::new_other("EOF", PARSING_WHAT_STMT)),
            next_token: None,
        },

        Test {
            test_name: "let",
            tokens: vec![
                Token::Let,
                Token::new_ident("x"),
                Token::Assign,
                Token::new_int(5),
                Token::Semicolon,
            ],
            expected: Ok(Statement::new_let(IdentifierExpression::new("x"), Expression::new_int(5))),
            next_token: None,
        },

        Test {
            test_name: "return",
            tokens: vec![
                Token::Return,
                Token::new_int(5),
                Token::Semicolon,
            ],
            expected: Ok(Statement::new_return(Expression::new_int(5))),
            next_token: None,
        },
    ];

    test_parser(&tests, |iter| Statement::parse(iter));
}

#[test]
fn test_let_statements() {
    type Test = ParsingTest<Statement>;

    let tests = [
        Test {
            test_name: "test_1",
            tokens: vec![
                Token::Let,
                Token::new_ident("x"),
                Token::Assign,
                Token::new_int(5),
                Token::Semicolon,
            ],
            expected: Ok(Statement::new_let(IdentifierExpression::new("x"), Expression::new_int(5))),
            next_token: None,
        },

        Test {
            test_name: "test_2",
            tokens: vec![
                Token::Let,
                Token::new_ident("y"),
                Token::Assign,
                Token::new_int(10),
                Token::Semicolon,
            ],
            expected: Ok(Statement::new_let(IdentifierExpression::new("y"), Expression::new_int(10))),
            next_token: None,
        },

        Test {
            test_name: "test_3",
            tokens: vec![
                Token::Let,
                Token::new_ident("foobar"),
                Token::Assign,
                Token::new_int(838383),
                Token::Semicolon,
            ],
            expected: Ok(Statement::new_let(IdentifierExpression::new("foobar"), Expression::new_int(838383))),
            next_token: None,
        },

        Test {
            test_name: "error_missing_assign",
            tokens: vec![
                Token::Let,
                Token::new_ident("x"),
                Token::Bang,
                Token::new_int(838383),
                Token::Semicolon
            ],
            expected: Err(ParsingError::new_unexpected(Some(&Token::Bang), vec![Token::Assign], "let statement")),
            next_token: Some(Token::Bang),
        },

        Test {
            test_name: "error_missing_semicolon",
            tokens: vec![
                Token::Let,
                Token::new_ident("x"),
                Token::Assign,
                Token::new_int(838383),
            ],
            expected: Err(ParsingError::new_unexpected(None, vec![Token::Semicolon], "let statement")),
            next_token: None,
        },
    ];

    test_parser(&tests, |iter| Statement::parse(iter));
}

#[test]
fn test_return_statements() {
    type Test = ParsingTest<Statement>;

    let tests = [
        Test {
            test_name: "test_1",
            tokens: vec![
                Token::Return,
                Token::new_int(5),
                Token::Semicolon,
            ],
            expected: Ok(Statement::new_return(Expression::new_int(5))),
            next_token: None,
        },

        Test {
            test_name: "test_2",
            tokens: vec![
                Token::Return,
                Token::new_int(10),
                Token::Semicolon,
            ],
            expected: Ok(Statement::new_return(Expression::new_int(10))),
            next_token: None,
        },

        Test {
            test_name: "test_3",
            tokens: vec![
                Token::Return,
                Token::new_int(993322),
                Token::Semicolon,
            ],
            expected: Ok(Statement::new_return(Expression::new_int(993322))),
            next_token: None,
        },

        Test {
            test_name: "error_missing_semicolon",
            tokens: vec![
                Token::Return,
                Token::new_int(993322),
            ],
            expected: Err(ParsingError::new_unexpected(None, vec![Token::Semicolon], "return statement")),
            next_token: None,
        },
    ];

    test_parser(&tests, |iter| Statement::parse(iter));
}

#[test]
fn test_block_statement() {
    type Test = ParsingTest<Statement>;

    let tests = vec![
        Test {
            test_name: "2stmts",
            tokens: vec![
                Token::LeftCurly,
                Token::Let, Token::new_ident("x"), Token::Assign, Token::new_int(5), Token::Semicolon,
                Token::new_ident("x"), Token::LessThan, Token::new_int(3), Token::Semicolon,
                Token::RightCurly,
            ],
            expected: Ok(Statement::new_block(
                BlockStatement::new(vec![
                    Statement::new_let(IdentifierExpression::new("x"), Expression::new_int(5)),
                    Statement::new_expr(Expression::new_infix(InfixOperator::LessThan, Expression::new_ident("x"), Expression::new_int(3))),
                ]))),
            next_token: None,
        },
        Test {
            test_name: "1stmt",
            tokens: vec![
                Token::LeftCurly,
                Token::new_ident("x"), Token::LessThan, Token::new_int(3), Token::Semicolon,
                Token::RightCurly,
            ],
            expected: Ok(Statement::new_block(
                BlockStatement::new(vec![
                    Statement::new_expr(Expression::new_infix(InfixOperator::LessThan, Expression::new_ident("x"), Expression::new_int(3))),
                ]))),
            next_token: None,
        },
        Test {
            test_name: "empty",
            tokens: vec![
                Token::LeftCurly,
                Token::RightCurly,
            ],
            expected: Ok(Statement::new_block(BlockStatement::new(vec![]))),
            next_token: None,
        },
        Test {
            test_name: "error_unclosed",
            tokens: vec![
                Token::LeftCurly,
                Token::Let, Token::new_ident("x"), Token::Assign, Token::new_int(5), Token::Semicolon,
                Token::new_ident("x"), Token::LessThan, Token::new_int(3), Token::Semicolon,
            ],
            expected: Err(ParsingError::new_other("unclosed curly brace", PARSING_WHAT_BLOCK_STMT)),
            next_token: None,
        },
    ];

    test_parser(&tests, |iter| Statement::parse(iter));
}
