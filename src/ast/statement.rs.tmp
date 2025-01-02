use std::iter::Peekable;
use std::fmt::{Display, Formatter};
use crate::token::Token;
use crate::ast::{
    TokenIter,
    Expression,
    IdentifierExpression,
    ParsingError
};

mod parsing;
pub use parsing::*;
mod display;
pub use display::*;
mod block_statement;
pub use block_statement::*;
mod tests;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    Let{ identifier: IdentifierExpression, expression: Expression},
    Return { expression: Expression},
    Expression {expression: Expression},
    Block { body: BlockStatement },
    /*
    IfElse(IfElseStatement),
    Empty,
    None,
    */
}

//builders
impl Statement {
    pub fn new_let(identifier: IdentifierExpression, expression: Expression) -> Statement{
        Statement::Let{ identifier, expression }
    }
    pub fn new_return(expression: Expression) -> Statement {
        Statement::Return { expression }
    }
    pub fn new_expr(expression: Expression) -> Statement {
        Statement::Expression { expression }
    }
    pub fn new_block(block_statement: BlockStatement) -> Statement {
        Statement::Block { body: block_statement }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BlockStatement {
    body: Vec<Statement>,
}

impl BlockStatement {
    pub fn new(body: Vec<Statement>) -> BlockStatement {
        BlockStatement { body }
    }

    pub fn len(&self) -> usize {
        self.body.len()
    }
}
