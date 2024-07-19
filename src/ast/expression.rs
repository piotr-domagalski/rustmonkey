#[derive(Debug)]
pub enum Expression {
    Identifier(IdentifierExpression),
    Literal(LiteralExpression),
    Prefix(Box<PrefixExpression>),
    /*
    Infix(InfixExpression),
    Call(CallExpression),
    If(IfExpression),
    */
}

#[derive(Debug)]
pub struct IdentifierExpression{
    pub value: String,
}

#[derive(Debug)]
pub enum LiteralExpression{
    Integer(i64),
    Bool(bool),
}

#[derive(Debug)]
pub enum PrefixExpression{
    Inverse(Expression),
    Negation(Expression),
}
