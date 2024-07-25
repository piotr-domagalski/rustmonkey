#![allow(unused)]

use crate::token::Token;
use crate::ast::TokenIter;
use std::iter::Peekable;
use std::fmt::{Formatter, Display};

mod operators;
pub use operators::*;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    Identifier { identifier_expression: IdentifierExpression},
    Literal { literal: Literal},
    Prefix { operator: PrefixOperator, expression: Box<Expression> },
    Infix { operator: InfixOperator, left: Box<Expression>, right: Box<Expression> },
    /*
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
    pub fn new_infix(operator: InfixOperator, left: Expression, right: Expression) -> Expression {
        Expression::Infix { operator, left: Box::new(left), right: Box::new(right)}
    }
}

//parsing
impl Expression {
    pub fn parse<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Expression, &'static str> {
        Self::parse_with_precedence(iter, Precedence::Lowest)
    }

    pub fn parse_with_precedence<I: TokenIter>(iter: &mut Peekable<I>, precedence: Precedence) -> Result<Expression, &'static str> {
        let mut left = match iter.peek() {
            Some(Token::Identifier(_)) => Expression::Identifier {identifier_expression: IdentifierExpression::parse(iter)? },
            Some(Token::Integer(_)) => Expression::Literal {literal: Literal::parse(iter)? },
            Some(Token::Bang) | Some(Token::Minus) => Expression::parse_prefix_expression(iter)?,
            _ => return Err("unimplemented expression type")
        };

        loop {
            let next_precedence = match iter.peek() {
                Some(Token::Semicolon) | None => return Ok(left),
                Some(token) => {
                    match InfixOperator::parse(token) {
                        Ok(op) => op.precedence(),
                        Err(_) => Precedence::Lowest,
                    }
                }
            };
            if precedence >= next_precedence {
                return Ok(left);
            }

            left = Self::parse_infix_expression(iter, left)?;
        }
        Ok(left)
    }

    fn parse_prefix_expression<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Expression, &'static str> {
        let operator = PrefixOperator::parse(iter)?;
        iter.next(); // operator parsing doesn't consume the token - do it manually
        Ok(Expression::new_prefix(operator, Expression::parse_with_precedence(iter, Precedence::Prefix)?))
    }

    fn parse_infix_expression<I: TokenIter>(iter: &mut Peekable<I>, left: Expression) -> Result<Expression, &'static str> {
        if let Some(token) = iter.next() {
            let operator = InfixOperator::parse(&token)?;  
            let right = Self::parse_with_precedence(iter, operator.precedence())?;

            Ok(Expression::Infix{operator, left: Box::new(left), right: Box::new(right)})
        } else {
            Err("expected operator token")
        } 
    }
}
impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            Expression::Identifier{identifier_expression} =>
                write!(f, "{}", identifier_expression),
            Expression::Literal{literal} => 
                write!(f, "{}", literal),
            Expression::Prefix{operator, expression} => 
                write!(f, "({}{})", operator, expression),
            Expression::Infix{operator, left, right} =>
                write!(f, "({} {} {})", left, operator, right),
        }
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
    pub fn parse<I: TokenIter>(iter: &mut Peekable<I>) -> Result<IdentifierExpression, &'static str> {
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

impl Display for IdentifierExpression {
    fn fmt (&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.identifier)
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

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            Literal::Integer(int) => write!(f, "{}", int),
            Literal::Bool(boolean) => write!(f, "{}", boolean),
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
            let tokens = [Token::Integer(5), test.token, Token::Integer(5), Token::Semicolon];
            let expected = Expression::new_infix(test.expected, Expression::new_int(5), Expression::new_int(5));

            let mut iterator = tokens.into_iter().peekable();
            let parsed = Expression::parse(&mut iterator).expect("Hardcoded tokens shouldn't fail to parse");

            assert_eq!(parsed, expected);
            assert_eq!(iterator.next(), Some(Token::Semicolon));
        }
    }
    #[test]
    fn test_complex_expressions() {
        struct Test {
            input: &'static str,
            expected: &'static str,
        }
        let tests = [
            Test {
                input: "-a",
                expected: "(-a)",
            },
            Test {
                input: "-a - b",
                expected: "((-a) - b)",
            },
            Test {
                input: "-a * b",
                expected: "((-a) * b)",
            },
            Test {
                input: "!-a",
                expected: "(!(-a))",
            },
            Test {
                input: "a + b + c",
                expected: "((a + b) + c)",
            },
            Test {
                input: "a + b - c",
                expected: "((a + b) - c)",
            },
            Test {
                input: "a * b * c",
                expected: "((a * b) * c)",
            },
            Test {
                input: "a * b / c",
                expected: "((a * b) / c)",
            },
            Test {
                input: "a + b / c",
                expected: "(a + (b / c))",
            },
            Test {
                input: "a + b * c + d / e - f",
                expected: "(((a + (b * c)) + (d / e)) - f)",
            },
            /*
            Test {
                input: "3 + 4; -5 * 5",
                expected: "(3 + 4)((-5) * 5)",
            },
            */
            Test {
                input: "5 > 4 == 3 < 4",
                expected: "((5 > 4) == (3 < 4))",
            },
            Test {
                input: "5 < 4 != 3 > 4",
                expected: "((5 < 4) != (3 > 4))",
            },
            Test {
                input: "3 + 4 * 5 == 3 * 1 + 4 * 5",
                expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            },
            Test {
                input: "3 + 4 * 5 == 3 * 1 + 4 * 5",
                expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            },
        ];

        for test in tests {
            let lexer = crate::lexer::Lexer::new(test.input);
            let parsed = Expression::parse(&mut lexer.peekable());
            assert!(parsed.is_ok(), "failed to parse \'{}\': {:?}", test.input, parsed);
            let parsed = parsed.expect("if not ok, assert above panics");
            let formatted = parsed.to_string();

            assert_eq!(formatted, test.expected, "\ninput: \"{}\" \nparsed: {:#?}", test.input, parsed);
        }
    }
}
