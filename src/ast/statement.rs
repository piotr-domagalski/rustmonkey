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

