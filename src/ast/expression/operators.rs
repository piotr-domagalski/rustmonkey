#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum PrefixOperator {
    Inverse,
    Negation,
}
impl PrefixOperator {
    pub fn precedence(&self) -> Precedence {
        Precedence::Prefix
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
