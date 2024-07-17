#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>
}

#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
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
pub enum Expression {
    Identifier(IdentifierExpression),
    Literal(LiteralExpression),
    /*
    Prefix(PrefixExpression),
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
    Bool(i64),
}
