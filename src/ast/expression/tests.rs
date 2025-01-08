#![cfg(test)]
use super::*;
use super::parsing_error_consts::*;

use crate::token::Token;
use crate::ast::ParsingError;

use crate::testing_common::{ParsingTest, test_parser};

#[test]
fn test_identifier_expression() {
    type Test = ParsingTest<Expression>;

    let tests = vec![
        Test {
            test_name: "test_1",
            tokens: vec![Token::new_ident("foobar"), Token::Semicolon],
            expected: Ok(Expression::new_ident("foobar")),
            next_token: Some(Token::Semicolon)
        }
    ];

    test_parser(&tests, | iter | Expression::parse(iter));
}

#[test]
fn test_literal_expression() {
    type Test = ParsingTest<Expression>;

    // TODO: needs more exhaustive tests
    let tests = [
        Test {
            test_name: "integer",
            tokens: vec![Token::new_int(5), Token::Semicolon],
            expected: Ok(Expression::new_int(5)),
            next_token: Some(Token::Semicolon),
        },
        Test {
            test_name: "bool",
            tokens: vec![Token::new_bool(true), Token::Semicolon],
            expected: Ok(Expression::new_bool(true)),
            next_token: Some(Token::Semicolon),
        },
        Test {
            test_name: "function_2params_1stmt",
            tokens: vec![
                Token::Function, Token::LeftRound, Token::new_ident("x"), Token::Comma, Token::new_ident("y"), Token::RightRound,
                Token::LeftCurly,
                Token::new_ident("x"), Token::Plus, Token::new_ident("y"), Token::Semicolon,
                Token::RightCurly,
            ],
            expected: Ok(Expression::new_fn(
                vec![IdentifierExpression::new("x"), IdentifierExpression::new("y")],
                BlockStatement::parse(
                    &mut vec![
                        Token::LeftCurly,
                        Token::new_ident("x"), Token::Plus, Token::new_ident("y"), Token::Semicolon,
                        Token::RightCurly
                    ].into_iter().peekable()
                ).expect("hardcoded tokens shouldn't fail to parse")
            )),
            next_token: None,
        },

        Test {
            test_name: "function_empty_empty",
            tokens: vec![Token::Function, Token::LeftRound, Token::RightRound,
                Token::LeftCurly, Token::RightCurly, Token::Semicolon
            ],
            expected: Ok(Expression::new_fn(
                vec![],
                BlockStatement::parse(
                    &mut vec![
                        Token::LeftCurly,
                        Token::RightCurly
                    ].into_iter().peekable()
                ).expect("hardcoded tokens shouldn't fail to parse")
            )),
            next_token: Some(Token::Semicolon),
        },

        Test {
            test_name: "function_1param_empty",
            tokens: vec![Token::Function, Token::LeftRound,
                    Token::new_ident("x"),
                Token::RightRound,
                Token::LeftCurly, Token::RightCurly, Token::Semicolon
            ],
            expected: Ok(Expression::new_fn(
                vec![
                    IdentifierExpression::new("x"),
                ],
                BlockStatement::parse(
                    &mut vec![
                        Token::LeftCurly,
                        Token::RightCurly
                    ].into_iter().peekable()
                ).expect("hardcoded tokens shouldn't fail to parse")
            )),
            next_token: Some(Token::Semicolon),
        },

        Test {
            test_name: "function_3params_empty",
            tokens: vec![Token::Function, Token::LeftRound,
                    Token::new_ident("x"), Token::Comma,
                    Token::new_ident("y"), Token::Comma,
                    Token::new_ident("z"),
                Token::RightRound,
                Token::LeftCurly, Token::RightCurly, Token::Semicolon
            ],
            expected: Ok(Expression::new_fn(
                vec![
                    IdentifierExpression::new("x"),
                    IdentifierExpression::new("y"),
                    IdentifierExpression::new("z"),
                ],
                BlockStatement::parse(
                    &mut vec![
                        Token::LeftCurly,
                        Token::RightCurly
                    ].into_iter().peekable()
                ).expect("hardcoded tokens shouldn't fail to parse")
            )),
            next_token: Some(Token::Semicolon),
        },

        Test {
            test_name: "function_error_comma",
            tokens: vec![Token::Function, Token::LeftRound,
                    Token::new_ident("x"), Token::Comma,
                Token::RightRound,
                Token::LeftCurly, Token::RightCurly, Token::Semicolon
            ],
            expected: Err(ParsingError::new_unexpected(Some(&Token::RightRound), vec![Token::AnIdentifier], PARSING_WHAT_FN_LIT)),
            next_token: Some(Token::LeftCurly),
        },
    ];

    test_parser(&tests, | iter | Expression::parse(iter));
}

#[test]
fn test_prefix_expressions() {
    type Test = ParsingTest<Expression>;

    let tests = [
        Test {
            test_name: "arithmetic_inverse",
            tokens: vec![Token::Minus, Token::new_int(5), Token::Semicolon],
            expected: Ok(Expression::new_prefix(PrefixOperator::Inverse, Expression::new_int(5))),
            next_token: Some(Token::Semicolon),
        },
        Test {
            test_name: "bool_negation",
            tokens: vec![Token::Bang, Token::new_int(15), Token::Semicolon],
            expected: Ok(Expression::new_prefix(PrefixOperator::Negation, Expression::new_int(15))),
            next_token: Some(Token::Semicolon),
        },
    ];

    test_parser(&tests, | iter | Expression::parse(iter));
}

#[test]
fn test_infix_expressions() {
    struct Test {
        test_name: &'static str,
        token: Token,
        expected: InfixOperator,
    }

    let tests = [
        Test {
            test_name: "plus",
            token: Token::Plus,
            expected: InfixOperator::Add,
        },
        Test {
            test_name: "minus",
            token: Token::Minus,
            expected: InfixOperator::Sub,
        },
        Test {
            test_name: "asterisk",
            token: Token::Asterisk,
            expected: InfixOperator::Mul,
        },
        Test {
            test_name: "slash",
            token: Token::Slash,
            expected: InfixOperator::Div,
        },
        Test {
            test_name: "less_than",
            token: Token::LessThan,
            expected: InfixOperator::LessThan,
        },
        Test {
            test_name: "greater_than",
            token: Token::GreaterThan,
            expected: InfixOperator::GreaterThan,
        },
        Test {
            test_name: "equals",
            token: Token::Equals,
            expected: InfixOperator::Equals,
        },
        Test {
            test_name: "not_equals",
            token: Token::NotEquals,
            expected: InfixOperator::NotEquals,
        },
    ];

    let tests = tests.map(|Test{test_name, token, expected}| {
        ParsingTest::<Expression> {
            test_name,
            tokens: vec![Token::new_int(5), token, Token::new_int(5), Token::Semicolon],
            expected: Ok(Expression::new_infix(expected, Expression::new_int(5), Expression::new_int(5))),
            next_token: Some(Token::Semicolon),
        }
    });

    test_parser(&tests, | iter | Expression::parse(iter));
}

#[test]
fn test_if_expressions() {
    type Test = ParsingTest<Expression>;

    let tests = [
        Test {
            test_name: "if",
            tokens: vec![
                Token::If,
                    Token::LeftRound, Token::new_ident("x"), Token::GreaterThan, Token::new_ident("y"), Token::RightRound,
                Token::LeftCurly,
                    Token::new_ident("x"), Token::Semicolon,
                Token::RightCurly,
            ],
            expected: Ok(Expression::new_if(
                Expression::new_infix(InfixOperator::GreaterThan, Expression::new_ident("x"), Expression::new_ident("y")),
                BlockStatement::parse(
                    &mut vec![
                        Token::LeftCurly,
                            Token::new_ident("x"), Token::Semicolon,
                        Token::RightCurly,
                    ].into_iter().peekable()
                ).expect("hardcoded tokens shouldn't fail to parse"),
                None
            )),
            next_token: None,
        },

        Test {
            test_name: "if_else",
            tokens: vec![
                Token::If,
                    Token::LeftRound, Token::new_ident("x"), Token::GreaterThan, Token::new_ident("y"), Token::RightRound,
                Token::LeftCurly,
                    Token::new_ident("x"), Token::Semicolon,
                Token::RightCurly,
                Token::Else,
                Token::LeftCurly,
                    Token::new_ident("y"), Token::Semicolon,
                Token::RightCurly,
            ],
            expected: Ok(Expression::new_if(
                Expression::new_infix(InfixOperator::GreaterThan, Expression::new_ident("x"), Expression::new_ident("y")),
                BlockStatement::parse(
                    &mut vec![
                        Token::LeftCurly,
                            Token::new_ident("x"), Token::Semicolon,
                        Token::RightCurly,
                    ].into_iter().peekable()
                ).expect("hardcoded tokens shouldn't fail to parse"),
                Some(BlockStatement::parse(
                    &mut vec![
                        Token::LeftCurly,
                            Token::new_ident("y"), Token::Semicolon,
                        Token::RightCurly,
                    ].into_iter().peekable()
                ).expect("hardcoded tokens shouldn't fail to parse"))
            )),
            next_token: None,
        },
    ];

    test_parser(&tests, |iter| Expression::parse(iter));
}

#[test]
fn test_call_expressions() {
    type Test = ParsingTest<Expression>;

    let tests = [
        Test {
            test_name: "test_1",
            tokens: vec![
                Token::new_ident("add"), Token::LeftRound,
                    Token::new_int(1), Token::Comma,
                    Token::new_int(2), Token::Asterisk, Token::new_int(3), Token::Comma,
                    Token::new_int(4), Token::Plus, Token::new_int(5), Token::Comma,
                Token::RightRound,
                Token::Semicolon
            ],
            expected: Ok(Expression::new_call(
                Expression::new_ident("add"),
                vec![
                    Expression::new_int(1),
                    Expression::new_infix(
                        InfixOperator::Mul,
                        Expression::new_int(2),
                        Expression::new_int(3),
                    ),
                    Expression::new_infix(
                        InfixOperator::Add,
                        Expression::new_int(4),
                        Expression::new_int(5),
                    ),
                ]
            )),
            next_token: Some(Token::Semicolon),
        },
    ];

    test_parser(&tests, |iter| Expression::parse(iter));
}

#[test]
fn test_complex_expressions() {
    struct Test {
        test_name: &'static str,
        input: &'static str,
        expected: Result<&'static str, ParsingError>,
        next_token: Option<Token>,
    }

    let tests = [
        Test {
            test_name: "simple_1",
            input: "-a",
            expected: Ok("(-a)"),
            next_token: None,
        },
        Test {
            test_name: "simple_2",
            input: "-a - b",
            expected: Ok("((-a) - b)"),
            next_token: None,
        },
        Test {
            test_name: "simple_3",
            input: "-a * b",
            expected: Ok("((-a) * b)"),
            next_token: None,
        },
        Test {
            test_name: "simple_4",
            input: "!-a",
            expected: Ok("(!(-a))"),
            next_token: None,
        },
        Test {
            test_name: "simple_5",
            input: "a + b + c",
            expected: Ok("((a + b) + c)"),
            next_token: None,
        },
        Test {
            test_name: "simple_6",
            input: "a + b - c",
            expected: Ok("((a + b) - c)"),
            next_token: None,
        },
        Test {
            test_name: "simple_7",
            input: "a * b * c",
            expected: Ok("((a * b) * c)"),
            next_token: None,
        },
        Test {
            test_name: "simple_8",
            input: "a * b / c",
            expected: Ok("((a * b) / c)"),
            next_token: None,
        },
        Test {
            test_name: "simple_9",
            input: "a + b / c",
            expected: Ok("(a + (b / c))"),
            next_token: None,
        },
        Test {
            test_name: "simple_10",
            input: "a + b * c + d / e - f",
            expected: Ok("(((a + (b * c)) + (d / e)) - f)"),
            next_token: None,
        },
        Test {
            test_name: "simple_11",
            input: "5 > 4 == 3 < 4",
            expected: Ok("((5 > 4) == (3 < 4))"),
            next_token: None,
        },
        Test {
            test_name: "simple_12",
            input: "5 < 4 != 3 > 4",
            expected: Ok("((5 < 4) != (3 > 4))"),
            next_token: None,
        },
        Test {
            test_name: "simple_13",
            input: "3 + 4 * 5 == 3 * 1 + 4 * 5",
            expected: Ok("((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
            next_token: None,
        },
        Test {
            test_name: "simple_15",
            input: "3 + 4 * 5 == 3 * 1 + 4 * 5",
            expected: Ok("((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
            next_token: None,
        },

        Test {
            test_name: "boolean_1",
            input: "true",
            expected: Ok("true"),
            next_token: None,
        },
        Test {
            test_name: "boolean_2",
            input: "false",
            expected: Ok("false"),
            next_token: None,
        },
        Test {
            test_name: "boolean_3",
            input: "3 > 5 == false",
            expected: Ok("((3 > 5) == false)"),
            next_token: None,
        },
        Test {
            test_name: "boolean_4",
            input: "3 < 5 == true",
            expected: Ok("((3 < 5) == true)"),
            next_token: None,
        },

        Test {
            test_name: "parenthesis_1",
            input: "1 + (2 + 3) + 4",
            expected: Ok("((1 + (2 + 3)) + 4)"),
            next_token: None,
        },
        Test {
            test_name: "parenthesis_2",
            input: "(5 + 5) * 2",
            expected: Ok("((5 + 5) * 2)"),
            next_token: None,
        },
        Test {
            test_name: "parenthesis_3",
            input: "2 / (5 + 5)",
            expected: Ok("(2 / (5 + 5))"),
            next_token: None,
        },
        Test {
            test_name: "parenthesis_4",
            input: "-(5 + 5)",
            expected: Ok("(-(5 + 5))"),
            next_token: None,
        },
        Test {
            test_name: "parenthesis_5",
            input: "!(true == true)",
            expected: Ok("(!(true == true))"),
            next_token: None,
        },

        Test {
            test_name: "parenthesis_error_unclosed_initial_1",
            input: "(1 + 2 + 1",
            expected: Err(ParsingError::new_other("unclosed parenthesis", PARSING_WHAT_GROUP_EXPR)),
            next_token: None,
        },
        Test {
            test_name: "parenthesis_error_unclosed_medial_1",
            input: "1 + (2 + 2",
            expected: Err(ParsingError::new_other("unclosed parenthesis", PARSING_WHAT_GROUP_EXPR)),
            next_token: None,
        },
        Test {
            test_name: "parenthesis_error_unclosed_initial_2",
            input: "((1 + 2) + 3 ",
            expected: Err(ParsingError::new_other("unclosed parenthesis", PARSING_WHAT_GROUP_EXPR)),
            next_token: None,
        },
        Test {
            test_name: "parenthesis_error_unclosed_medial_2",
            input: "1 + ((2 + 3) + 4 ",
            expected: Err(ParsingError::new_other("unclosed parenthesis", PARSING_WHAT_GROUP_EXPR)),
            next_token: None,
        },

        Test {
            test_name: "parenthesis_error_missing_medial_1",
            input: "1 + 2) * 6",
            expected: Err(ParsingError::new_other("missing opening parenthesis", PARSING_WHAT_EXPR)),
            next_token: Some(Token::RightRound),
        },
        Test {
            test_name: "parenthesis_error_missing_terminal_1",
            input: "1 + 2 * 6)",
            expected: Err(ParsingError::new_other("missing opening parenthesis", PARSING_WHAT_EXPR)),
            next_token: Some(Token::RightRound),
        },
        Test {
            test_name: "parenthesis_error_missing_medial_2",
            input: "(1) + 2) * 4",
            expected: Err(ParsingError::new_other("missing opening parenthesis", PARSING_WHAT_EXPR)),
            next_token: Some(Token::RightRound),
        },
        Test {
            test_name: "parenthesis_error_missing_medial_3",
            input: "(1 + 2)) * 5",
            expected: Err(ParsingError::new_other("missing opening parenthesis", PARSING_WHAT_EXPR)),
            next_token: Some(Token::RightRound),
        },
        Test {
            test_name: "parenthesis_error_missing_terminal_2",
            input: "(1 + 2 * 6))",
            expected: Err(ParsingError::new_other("missing opening parenthesis", PARSING_WHAT_EXPR)),
            next_token: Some(Token::RightRound),
        },

        Test {
            test_name: "if",
            input: "if (x > y) { x; }",
            expected: Ok("if (x > y) { block stmt len=1 }"),
            next_token: None,
        },
        Test {
            test_name: "if_else",
            input: "if (x > y) { x; } else { y; }",
            expected: Ok("if (x > y) { block stmt len=1 } else { block stmt len=1 }"),
            next_token: None,
        },
        Test {
            test_name: "if_without_semicolon",
            input: "if (x > y) { x }",
            expected: Ok("if (x > y) { block stmt len=1 }"),
            next_token: None,
        },
        Test {
            test_name: "if_else_without_semicolon",
            input: "if (x > y) { x } else { y }",
            expected: Ok("if (x > y) { block stmt len=1 } else { block stmt len=1 }"),
            next_token: None,
        },
    ];

    let tests: Vec<ParsingTest<String>> = tests.into_iter().map(|Test {test_name, input, expected, next_token}| {
        ParsingTest::<String> {
            test_name,
            tokens: crate::lexer::Lexer::new(input).collect::<Vec<Token>>(),
            expected: { expected.map(|s| s.to_string()) },
            next_token,
        }
    }).collect();

    test_parser(&tests, |iter| Expression::parse(iter).map(|expr| expr.to_string()));
}
