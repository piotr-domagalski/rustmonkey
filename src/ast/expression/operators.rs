use std::fmt::{Formatter, Display};
use std::iter::Peekable;
use crate::parsing::{Parsable, ParsingError, TokenIter, Token};

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum PrefixOperator {
    Inverse,
    Negation,
}

impl Parsable for PrefixOperator {
    fn parse<I: TokenIter>(iter: &mut Peekable<I>) -> Result<PrefixOperator, ParsingError> {
        Self::from_token(iter.next().as_ref())
    }
}
impl PrefixOperator {
    pub fn precedence(&self) -> Precedence {
        Precedence::Prefix
    }
    pub fn from_token(token: Option<&Token>) -> Result<PrefixOperator, ParsingError> {
        use PrefixOperator::*;
        match token {
            Some(Token::Minus) => Ok(Inverse),
            Some(Token::Bang) => Ok(Negation),
            _ => Err(ParsingError::new_unexpected(token, vec![Token::Bang, Token::Minus], "prefix operator")),
        }
    }
}

impl Display for PrefixOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use PrefixOperator::*;
        match &self {
            Inverse => write!(f, "-"),
            Negation => write!(f, "!"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum InfixOperator {
    Add,
    Sub,
    Mul,
    Div,
    LessThan,
    GreaterThan,
    Equals,
    NotEquals,
}

impl Parsable for InfixOperator {
    fn parse<I: TokenIter>(iter: &mut Peekable<I>) -> Result<InfixOperator, ParsingError> {
        Self::from_token(iter.next().as_ref())
    }
}
impl InfixOperator {
    pub fn precedence(&self) -> Precedence {
        use InfixOperator::*;
        match &self {
            Equals | NotEquals =>
                Precedence::Equals,
            LessThan | GreaterThan =>
                Precedence::LessGreater,
            Add | Sub =>
                Precedence::Sum,
            Mul | Div =>
                Precedence::Product,
        }
    }
    pub fn from_token(token: Option<&Token>) -> Result<InfixOperator, ParsingError> {
        use InfixOperator::*;
        match token {
            Some(Token::Plus) => Ok(Add),
            Some(Token::Minus) => Ok(Sub),
            Some(Token::Asterisk) => Ok(Mul),
            Some(Token::Slash) => Ok(Div),
            Some(Token::LessThan) => Ok(LessThan),
            Some(Token::GreaterThan) => Ok(GreaterThan),
            Some(Token::Equals) => Ok(Equals),
            Some(Token::NotEquals) => Ok(NotEquals),
            _ => Err(ParsingError::new_unexpected(
                token,
                vec![Token::Plus, Token::Minus, Token::Asterisk, Token::Slash, Token::LessThan, Token::GreaterThan, Token::Equals, Token::NotEquals],
                "infix operator"
            )),
        }
    }
}

impl Display for InfixOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use InfixOperator::*;
        let symbol = match &self {
            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",
            LessThan => "<",
            GreaterThan => ">",
            Equals => "==",
            NotEquals => "!=",
        };
        write!(f, "{}", symbol)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
pub enum Precedence{
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parsing::Token;
    use crate::testing_common::{ParsingTest, test_parser};
    #[test]
    fn test_precedence() {
        assert!(Precedence::Lowest < Precedence::Equals);
        assert!(Precedence::Equals < Precedence::LessGreater);
        assert!(Precedence::LessGreater < Precedence::Sum);
        assert!(Precedence::Sum < Precedence::Product);
        assert!(Precedence::Product < Precedence::Prefix);
        assert!(Precedence::Prefix < Precedence::Call);
    }

    #[test]
    fn test_infix_operator() {
        type Test = ParsingTest<InfixOperator>;

        let tests = [
            Test {
                test_name: "plus",
                tokens: vec![Token::Plus],
                expected: Ok(InfixOperator::Add),
                next_token: Some(Token::Plus),
            },

            Test {
                test_name: "error_invalid_token",
                tokens: vec![Token::Bang],
                expected: Err(ParsingError::new_unexpected(
                    Some(Token::Bang).as_ref(),
                    vec![Token::Plus, Token::Minus, Token::Asterisk, Token::Slash,
                        Token::LessThan, Token::GreaterThan, Token::Equals, Token::NotEquals],
                    "infix operator"
                )),
                next_token: Some(Token::Bang),
            },

            Test {
                test_name: "error_no_token",
                tokens: vec![],
                expected: Err(ParsingError::new_unexpected(
                    None,
                    vec![Token::Plus, Token::Minus, Token::Asterisk, Token::Slash,
                        Token::LessThan, Token::GreaterThan, Token::Equals, Token::NotEquals],
                    "infix operator"
                )),
                next_token: None,
            }
        ];

        let tests_from_token: Vec<_> = tests.clone().into_iter()
            .map(|test| Test {
                test_name: format!("from_token_{}", test.test_name).leak(),
                ..test })
            .collect();

        let tests_parse: Vec<_> = tests.clone().into_iter()
            .map(|test| Test {
                test_name: format!("parse_{}", test.test_name).leak(),
                next_token: None,
                ..test })
            .collect();

        test_parser(&tests_from_token, |iter| InfixOperator::from_token(iter.peek()));
        test_parser(&tests_parse, |iter| InfixOperator::parse(iter));
    }

    #[test]
    fn test_prefix_operator() {
        type Test = ParsingTest<PrefixOperator>;

        let tests = vec![
            Test {
                test_name: "minus",
                tokens: vec![Token::Minus],
                expected: Ok(PrefixOperator::Inverse),
                next_token: Some(Token::Minus)
            },
            Test {
                test_name: "bang",
                tokens: vec![Token::Bang],
                expected: Ok(PrefixOperator::Negation),
                next_token: Some(Token::Bang)
            },
            Test {
                test_name: "error_invalid_token",
                tokens: vec![Token::new_ident("foobaz")],
                expected: Err(ParsingError::new_unexpected(
                    Some(&Token::new_ident("foobaz")),
                    vec![Token::Bang, Token::Minus],
                    "prefix operator")),
                next_token: Some(Token::new_ident("foobaz")),
            },
            Test {
                test_name: "error_no_token",
                tokens: vec![],
                expected: Err(ParsingError::new_unexpected(
                    None,
                    vec![Token::Bang, Token::Minus],
                    "prefix operator")),
                next_token: None
            }
        ];

        let tests_from_token: Vec<_> = tests.clone().into_iter()
            .map(|test| Test {
                test_name: format!("from_token_{}", test.test_name).leak(),
                ..test })
            .collect();

        let tests_parse: Vec<_> = tests.clone().into_iter()
            .map(|test| Test {
                test_name: format!("parse_{}", test.test_name).leak(),
                next_token: None,
                ..test })
            .collect();

        test_parser(&tests_from_token, |iter| PrefixOperator::from_token(iter.peek()));
        test_parser(&tests_parse, |iter| PrefixOperator::parse(iter));
    }
}
