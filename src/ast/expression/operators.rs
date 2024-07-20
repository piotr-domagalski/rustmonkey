use std::fmt::{Formatter, Display};

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum PrefixOperator {
    Inverse,
    Negation,
}
impl PrefixOperator {
    pub fn precedence(&self) -> Precedence {
        Precedence::Prefix
    }
    pub fn parse(token: &crate::token::Token) -> Result<PrefixOperator, &'static str> {
        use PrefixOperator::*;
        use crate::token::Token;
        match token {
            Token::Minus => Ok(Inverse),
            Token::Bang => Ok(Negation),
            _ => Err("Expected ! or - operator"),
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
    pub fn parse(token: &crate::token::Token) -> Result<InfixOperator, &'static str>{
        use InfixOperator::*;
        use crate::token::Token;
        match token {
            Token::Plus => Ok(Add),
            Token::Minus => Ok(Sub),
            Token::Asterisk => Ok(Mul),
            Token::Slash => Ok(Div),
            Token::LessThan => Ok(LessThan),
            Token::GreaterThan => Ok(GreaterThan),
            Token::Equals => Ok(Equals),
            Token::NotEquals => Ok(NotEquals),
            _ => Err("invalid operator"),
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
    #[test]
    fn test_precedence() {
        assert!(Precedence::Lowest < Precedence::Equals);
        assert!(Precedence::Equals < Precedence::LessGreater);
        assert!(Precedence::LessGreater < Precedence::Sum);
        assert!(Precedence::Sum < Precedence::Product);
        assert!(Precedence::Product < Precedence::Prefix);
        assert!(Precedence::Prefix < Precedence::Call);
    } 
}
