#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>
}

#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    /*
    IfElse(IfElseStatement),
    Expression(ExpressionStatement),
    Empty,
    None,
    */
}

#[derive(Debug)]
pub struct LetStatement {
    pub identifier: IdentifierExpression,
    pub value: Expression,
}

#[derive(Debug)]
pub struct ReturnStatement{
    pub value: Expression,
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub value: Expression,
}

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
