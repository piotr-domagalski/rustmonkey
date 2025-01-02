use std::iter::Peekable;
use std::fmt::{Display, Formatter};
use crate::token::Token;
use crate::ast::{
    TokenIter,
    Expression,
    IdentifierExpression,
    ParsingError,
    Statement,
    BlockStatement,
};

impl Display for Statement {
    fn fmt (&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Let { identifier, expression } =>
                write!(f, "let {} = {};", identifier, expression),
            Statement::Return { expression } =>
                write!(f, "return {};", expression),
            Statement::Expression { expression } =>
                write!(f, "{}", expression),
            Statement::Block { body } =>
                write!(f, "{}", body),
        }
    }
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ block stmt len={} }}", self.len())
    }
}
