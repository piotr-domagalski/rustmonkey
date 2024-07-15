#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Illegal(String),

    Identifier(String),

    //literals
    Integer(i64),

    //operators
    Assign,
    Plus,

    //separators
    Comma,
    Semicolon,

    //brackets
    LeftRound,
    RightRound,
    LeftSquare,
    RightSquare,
    LeftCurly,
    RightCurly,

    //keywords
    Function,
    Let,
}
