use crate::token::Token;
use crate::ast::TokenIter;
use std::iter::Peekable;

#[derive(Debug)]
pub enum Expression {
    Identifier { identifier: String},
    Literal { literal_expression: LiteralExpression },
    Prefix { operator: String, expression: Box<Expression>},
    /*
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Call(CallExpression),
    If(IfExpression),
    */
}
impl Expression {

    pub fn parse<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Expression, &'static str>
    {
        while iter.next_if(|tok| *tok != Token::Semicolon).is_some() {}
        iter.next();
        Ok(Expression::Identifier{
            identifier: String::from("haha fooled ya")
            }
        )
    }
}
#[derive(Debug)]
pub struct IdentifierExpression{
    pub identifier: String,
}

#[derive(Debug)]
pub enum LiteralExpression{
    Integer(i64),
    Bool(i64),
}
