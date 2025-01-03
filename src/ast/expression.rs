use crate::ast::BlockStatement;

mod parsing;
mod display;
mod operators;
mod tests;
pub use operators::*;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    Identifier { identifier_expression: IdentifierExpression},
    Literal { literal: Literal},
    Prefix { operator: PrefixOperator, expression: Box<Expression> },
    Infix { operator: InfixOperator, left: Box<Expression>, right: Box<Expression> },
    If { condition: Box<Expression>, consequence: BlockStatement, alternative: Option<BlockStatement> },
    Call { callable: Box<Expression>, arguments: Vec<Expression> },
}

//builders
impl Expression {
    #![allow(dead_code, reason = "these are used only in the test profile")]
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
    pub fn new_fn(parameters: Vec<IdentifierExpression>, body: BlockStatement) -> Expression {
        Expression::Literal { literal: Literal::new_fn(parameters, body) }
    }
    pub fn new_if(condition: Expression, consequence: BlockStatement, alternative: Option<BlockStatement>) -> Expression {
        Expression::If { condition: Box::new(condition), consequence, alternative }
    }
    pub fn new_call(callable: Expression, arguments: Vec<Expression>) -> Expression {
        Expression::Call { callable: Box::new(callable), arguments }
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Literal{
    Integer(i64),
    Bool(bool),
    Function {
        parameters: Vec<IdentifierExpression>,
        body: BlockStatement,
    }
}

impl Literal {
    pub fn new_int(integer: i64) -> Literal {
        Literal::Integer(integer)
    }
    pub fn new_bool(boolean: bool) -> Literal {
        Literal::Bool(boolean)
    }
    pub fn new_fn(parameters: Vec<IdentifierExpression>, body: BlockStatement) -> Literal {
        Literal::Function{ parameters, body }
    }
}
