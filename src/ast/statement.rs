use crate::ast::{
    Expression,
    IdentifierExpression,
};

mod parsing_what_consts {
    pub const PARSING_WHAT_STMT: &str = "statement";
    pub const PARSING_WHAT_BLOCK_STMT: &str = "block statement";
    pub const PARSING_WHAT_LET_STMT: &str = "let statement";
    pub const PARSING_WHAT_RETURN_STMT: &str = "return statement";
}

mod parsing;
mod display;
mod tests;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    Let{ identifier: IdentifierExpression, expression: Expression},
    Return { expression: Expression},
    Expression {expression: Expression},
    Block { body: BlockStatement },
}

impl Statement {
    pub fn new_let(identifier: IdentifierExpression, expression: Expression) -> Statement {
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
