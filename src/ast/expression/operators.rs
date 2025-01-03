use std::fmt::{Formatter, Display};
use std::iter::Peekable;
use crate::ast::{ParsingError, TokenIter};

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum PrefixOperator {
    Inverse,
    Negation,
}
impl PrefixOperator {
    pub fn precedence(&self) -> Precedence {
        Precedence::Prefix
    }
    pub fn parse<I: TokenIter>(iter: &mut Peekable<I>) -> Result<PrefixOperator, ParsingError> {
        use PrefixOperator::*;
        use crate::token::Token;
        match iter.peek() {
            Some(Token::Minus) => Ok(Inverse),
            Some(Token::Bang) => Ok(Negation),
            _ => Err(ParsingError::new_unexpected(iter.peek(), vec![Token::Bang, Token::Minus], "prefix operator")),
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
    pub fn parse<I: TokenIter>(iter: &mut Peekable<I>) -> Result<InfixOperator, ParsingError>{
        use InfixOperator::*;
        use crate::token::Token;
        match iter.peek() {
            Some(Token::Plus) => Ok(Add),
            Some(Token::Minus) => Ok(Sub),
            Some(Token::Asterisk) => Ok(Mul),
            Some(Token::Slash) => Ok(Div),
            Some(Token::LessThan) => Ok(LessThan),
            Some(Token::GreaterThan) => Ok(GreaterThan),
            Some(Token::Equals) => Ok(Equals),
            Some(Token::NotEquals) => Ok(NotEquals),
            _ => Err(ParsingError::new_unexpected(
                iter.peek(),
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
    use crate::token::Token;
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
        struct Test{
            input: Vec<Token>,
            expected: Result<InfixOperator, ParsingError>,
            iter_state: Option<Token>,
        }

        let tests = [
            //every operator
            Test {
                input: vec![Token::Plus],
                expected: Ok(InfixOperator::Add),
                iter_state: Some(Token::Plus),
            },

            //wrong token
            Test {
                input: vec![Token::Bang],
                expected: Err(ParsingError::new_unexpected(
                    Some(Token::Bang).as_ref(),
                    vec![Token::Plus, Token::Minus, Token::Asterisk, Token::Slash,
                        Token::LessThan, Token::GreaterThan, Token::Equals, Token::NotEquals],
                    "infix operator")),
                iter_state: Some(Token::Bang),
            },

            //no token
            Test {
                input: vec![],
                expected: Err(ParsingError::new_unexpected(
                    None,
                    vec![Token::Plus, Token::Minus, Token::Asterisk, Token::Slash,
                        Token::LessThan, Token::GreaterThan, Token::Equals, Token::NotEquals],
                    "infix operator")),
                iter_state: None,
            }
        ];

        for Test{input, expected, iter_state} in tests {
            let mut iter = input.into_iter().peekable();
            assert_eq!(InfixOperator::parse(&mut iter), expected);
            assert_eq!(iter.peek(), iter_state.as_ref());
        }
    }

    #[test]
    fn test_prefix_operator() {
        struct Test{
            input: Vec<Token>,
            expected: Result<PrefixOperator, ParsingError>,
            iter_state: Option<Token>
        }
        let tests = [
            Test {
                input: vec![Token::Minus],
                expected: Ok(PrefixOperator::Inverse),
                iter_state: Some(Token::Minus)
            },
            Test {
                input: vec![Token::Bang],
                expected: Ok(PrefixOperator::Negation),
                iter_state: Some(Token::Bang)
            },
            Test {
                input: vec![Token::Identifier("foobaz".to_string())],
                expected: Err(ParsingError::new_unexpected(
                    Some(&Token::Identifier("foobaz".to_string())),
                    vec![Token::Bang, Token::Minus],
                    "prefix operator")),
                iter_state: Some(Token::Identifier("foobaz".to_string())),
            },
            Test {

                input: vec![],
                expected: Err(ParsingError::new_unexpected(
                    None,
                    vec![Token::Bang, Token::Minus],
                    "prefix operator")),
                iter_state: None
            }
        ];

        for Test{input, expected, iter_state} in tests {
            let mut iter = input.into_iter().peekable();
            assert_eq!(PrefixOperator::parse(&mut iter), expected);
            assert_eq!(iter.peek(), iter_state.as_ref());
        }
    }
}
