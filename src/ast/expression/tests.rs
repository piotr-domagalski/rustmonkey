#![cfg(test)]
use super::*;
use super::parsing_error_consts::*;

use crate::token::Token;
use crate::ast::ParsingError;

#[test]
fn test_identifier_expression() {
    let input = [
        Token::new_ident("foobar"),
        Token::Semicolon,
    ];
    let expected = Expression::new_ident("foobar");

    let mut iterator = input.into_iter().peekable();
    let parsed = Expression::parse(&mut iterator).expect("Hardcoded tokens shouldn't fail to parse");

    assert_eq!(iterator.next(), Some(Token::Semicolon));
    assert_eq!(parsed, expected);
}

#[test]
fn test_literal_expression() {
    struct Test {
        tokens: Vec<Token>,
        expected: Result<Expression, ParsingError>,
        next_tok: Option<Token>,
    }

    let tests = [
        Test {
            tokens: vec![Token::new_int(5), Token::Semicolon],
            expected: Ok(Expression::new_int(5)),
            next_tok: Some(Token::Semicolon),
        },
        Test {
            tokens: vec![Token::new_bool(true), Token::Semicolon],
            expected: Ok(Expression::new_bool(true)),
            next_tok: Some(Token::Semicolon),
        },
        // TODO: needs more exhaustive tests
        Test {
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
            next_tok: None,
        },

        Test {
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
            next_tok: Some(Token::Semicolon),
        },

        Test {
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
            next_tok: Some(Token::Semicolon),
        },

        Test {
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
            next_tok: Some(Token::Semicolon),
        },

        Test {
            tokens: vec![Token::Function, Token::LeftRound,
                    Token::new_ident("x"), Token::Comma,
                Token::RightRound,
                Token::LeftCurly, Token::RightCurly, Token::Semicolon
            ],
            expected: Err(ParsingError::new_unexpected(Some(&Token::RightRound), vec![Token::AnIdentifier], PARSING_WHAT_FN_LIT)),
            next_tok: Some(Token::LeftCurly),
        },
    ];

    for Test { tokens, expected, next_tok } in tests {
        let mut iterator = tokens.into_iter().peekable();
        let parsed = Expression::parse(&mut iterator);

        assert_eq!(parsed, expected);
        assert_eq!(iterator.next(), next_tok);
    }
}

#[test]
fn test_prefix_expressions() {
    struct Test {
        tokens: [Token; 3],
        expected: Result<Expression, ParsingError>
    }

    let tests = [
        Test {
            tokens: [Token::Minus, Token::new_int(5), Token::Semicolon],
            expected: Ok(Expression::new_prefix(PrefixOperator::Inverse, Expression::new_int(5))),
        },
        Test {
            tokens: [Token::Bang, Token::new_int(15), Token::Semicolon],
            expected: Ok(Expression::new_prefix(PrefixOperator::Negation, Expression::new_int(15))),
        },
    ];

    for Test { tokens, expected } in tests {
        let mut iterator = tokens.clone().into_iter().peekable();
        let parsed = Expression::parse(&mut iterator);

        assert_eq!(iterator.next(), Some(Token::Semicolon), "input: {:?}, parsed: {:?}", tokens, parsed);
        assert_eq!(parsed, expected);
    }
}

#[test]
fn test_infix_expressions() {
    struct Test {
        token: Token,
        expected: InfixOperator,
    }

    let tests = [
        Test {
            token: Token::Plus,
            expected: InfixOperator::Add,
        },
        Test {
            token: Token::Minus,
            expected: InfixOperator::Sub,
        },
        Test {
            token: Token::Asterisk,
            expected: InfixOperator::Mul,
        },
        Test {
            token: Token::Slash,
            expected: InfixOperator::Div,
        },
        Test {
            token: Token::LessThan,
            expected: InfixOperator::LessThan,
        },
        Test {
            token: Token::GreaterThan,
            expected: InfixOperator::GreaterThan,
        },
        Test {
            token: Token::Equals,
            expected: InfixOperator::Equals,
        },
        Test {
            token: Token::NotEquals,
            expected: InfixOperator::NotEquals,
        },
    ];

    for test in tests {
        let tokens = [Token::new_int(5), test.token, Token::new_int(5), Token::Semicolon];
        let expected = Expression::new_infix(test.expected, Expression::new_int(5), Expression::new_int(5));

        let mut iterator = tokens.into_iter().peekable();
        let parsed = Expression::parse(&mut iterator).expect("Hardcoded tokens shouldn't fail to parse");

        assert_eq!(parsed, expected);
        assert_eq!(iterator.next(), Some(Token::Semicolon));
    }
}
#[test]
fn test_if_expressions() {
    struct Test {
        tokens: Vec<Token>,
        expected: Expression,
        next_token: Option<Token>,
    }

    let tests = [
        Test {
            tokens: vec![
                Token::If,
                    Token::LeftRound, Token::new_ident("x"), Token::GreaterThan, Token::new_ident("y"), Token::RightRound,
                Token::LeftCurly,
                    Token::new_ident("x"), Token::Semicolon,
                Token::RightCurly,
            ],
            expected: Expression::new_if(
                Expression::new_infix(InfixOperator::GreaterThan, Expression::new_ident("x"), Expression::new_ident("y")),
                BlockStatement::parse(
                    &mut vec![
                        Token::LeftCurly,
                            Token::new_ident("x"), Token::Semicolon,
                        Token::RightCurly,
                    ].into_iter().peekable()
                ).expect("hardcoded tokens shouldn't fail to parse"),
                None
            ),
            next_token: None,
        },

        Test {
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
            expected: Expression::new_if(
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
            ),
            next_token: None,
        },
    ];

    for Test {tokens, expected, next_token} in tests {
        let mut iterator = tokens.clone().into_iter().peekable();
        let parsed = Expression::parse(&mut iterator);

        assert!(parsed.is_ok());
        let parsed = parsed.expect("above assert should ensure this is the Ok variant");
        assert_eq!(expected, parsed, "expected: {}\nparsed: {}", expected, parsed);
        assert_eq!(iterator.next(), next_token, "input: {:?}, parsed: {:?}", tokens, parsed);

    }
}

#[test]
fn test_call_expressions() {
    struct Test {
        tokens: Vec<Token>,
        expected: Expression,
        next_token: Option<Token>,
    }

    let tests = [
        Test {
            tokens: vec![
                Token::new_ident("add"), Token::LeftRound,
                    Token::new_int(1), Token::Comma,
                    Token::new_int(2), Token::Asterisk, Token::new_int(3), Token::Comma,
                    Token::new_int(4), Token::Plus, Token::new_int(5), Token::Comma,
                Token::RightRound,
                Token::Semicolon
            ],
            expected: Expression::new_call(
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
            ),
            next_token: Some(Token::Semicolon),
        },
    ];

    for Test {tokens, expected, next_token} in tests {
        let mut iterator = tokens.clone().into_iter().peekable();
        let parsed = Expression::parse(&mut iterator);

        assert!(parsed.is_ok(), "expected ok, got {:?}", parsed);
        let parsed = parsed.expect("above assert should ensure this is the Ok variant");
        assert_eq!(expected, parsed, "expected: {}\nparsed: {}", expected, parsed);
        assert_eq!(iterator.next(), next_token, "input: {:?}, parsed: {:?}", tokens, parsed);

    }

}

#[test]
fn test_complex_expressions() {
    struct Test {
        input: &'static str,
        expected: Result<&'static str, ParsingError>,
    }
    let tests = [
        Test {
            input: "-a",
            expected: Ok("(-a)"),
        },
        Test {
            input: "-a - b",
            expected: Ok("((-a) - b)"),
        },
        Test {
            input: "-a * b",
            expected: Ok("((-a) * b)"),
        },
        Test {
            input: "!-a",
            expected: Ok("(!(-a))"),
        },
        Test {
            input: "a + b + c",
            expected: Ok("((a + b) + c)"),
        },
        Test {
            input: "a + b - c",
            expected: Ok("((a + b) - c)"),
        },
        Test {
            input: "a * b * c",
            expected: Ok("((a * b) * c)"),
        },
        Test {
            input: "a * b / c",
            expected: Ok("((a * b) / c)"),
        },
        Test {
            input: "a + b / c",
            expected: Ok("(a + (b / c))"),
        },
        Test {
            input: "a + b * c + d / e - f",
            expected: Ok("(((a + (b * c)) + (d / e)) - f)"),
        },
        /*
        Test {
            input: "3 + 4; -5 * 5",
            expected: Ok("(3 + 4)((-5) * 5)"),
        },
        */
        Test {
            input: "5 > 4 == 3 < 4",
            expected: Ok("((5 > 4) == (3 < 4))"),
        },
        Test {
            input: "5 < 4 != 3 > 4",
            expected: Ok("((5 < 4) != (3 > 4))"),
        },
        Test {
            input: "3 + 4 * 5 == 3 * 1 + 4 * 5",
            expected: Ok("((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
        },
        Test {
            input: "3 + 4 * 5 == 3 * 1 + 4 * 5",
            expected: Ok("((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
        },

        //boolean
        Test {
            input: "true",
            expected: Ok("true"),
        },
        Test {
            input: "false",
            expected: Ok("false"),
        },
        Test {
            input: "3 > 5 == false",
            expected: Ok("((3 > 5) == false)"),
        },
        Test {
            input: "3 < 5 == true",
            expected: Ok("((3 < 5) == true)"),
        },

        //brackets
        Test {
            input: "1 + (2 + 3) + 4",
            expected: Ok("((1 + (2 + 3)) + 4)"),
        },
        Test {
            input: "(5 + 5) * 2",
            expected: Ok("((5 + 5) * 2)"),
        },
        Test {
            input: "2 / (5 + 5)",
            expected: Ok("(2 / (5 + 5))"),
        },
        Test {
            input: "-(5 + 5)",
            expected: Ok("(-(5 + 5))"),
        },
        Test {
            input: "!(true == true)",
            expected: Ok("(!(true == true))"),
        },

        Test {
            input: "(1 + 2 + 1",
            expected: Err(ParsingError::new_other("unclosed parenthesis", PARSING_WHAT_GROUP_EXPR)),
        },
        Test {
            input: "1 + (2 + 2",
            expected: Err(ParsingError::new_other("unclosed parenthesis", PARSING_WHAT_GROUP_EXPR)),
        },
        Test {
            input: "((1 + 2) + 3 ",
            expected: Err(ParsingError::new_other("unclosed parenthesis", PARSING_WHAT_GROUP_EXPR)),
        },
        Test {
            input: "1 + ((2 + 3) + 4 ",
            expected: Err(ParsingError::new_other("unclosed parenthesis", PARSING_WHAT_GROUP_EXPR)),
        },

        Test {
            input: "1 + 2) * 6",
            expected: Err(ParsingError::new_other("missing opening parenthesis", PARSING_WHAT_EXPR)),
        },
        Test {
            input: "1 + 2 * 6)",
            expected: Err(ParsingError::new_other("missing opening parenthesis", PARSING_WHAT_EXPR)),
        },
        Test {
            input: "(1) + 2) * 4",
            expected: Err(ParsingError::new_other("missing opening parenthesis", PARSING_WHAT_EXPR)),
        },
        Test {
            input: "(1 + 2)) * 5",
            expected: Err(ParsingError::new_other("missing opening parenthesis", PARSING_WHAT_EXPR)),
        },
        Test {
            input: "(1 + 2 * 6))",
            expected: Err(ParsingError::new_other("missing opening parenthesis", PARSING_WHAT_EXPR)),
        },

        Test {
            input: "if (x > y) { x; }",
            expected: Ok("if (x > y) { block stmt len=1 }")
        },
        Test {
            input: "if (x > y) { x; } else { y; }",
            expected: Ok("if (x > y) { block stmt len=1 } else { block stmt len=1 }")
        },
        Test {
            input: "if (x > y) { x }",
            expected: Ok("if (x > y) { block stmt len=1 }")
        },
        Test {
            input: "if (x > y) { x } else { y }",
            expected: Ok("if (x > y) { block stmt len=1 } else { block stmt len=1 }")
        },
    ];

    for test in tests {
        let lexer = crate::lexer::Lexer::new(test.input);
        let parsed = Expression::parse(&mut lexer.peekable());
        let expected = match test.expected {
            Ok(str) => Ok(str.to_string()),
            Err(err) => Err(err),
        };

        let result = match parsed {
            Ok(expr) => Ok(expr.to_string()),
            Err(err) => Err(err),
        };

        assert_eq!(result, expected, "{}", test.input);
    }
}
