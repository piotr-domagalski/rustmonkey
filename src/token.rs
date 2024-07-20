#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    Illegal(String),

    Identifier(String),

    //literals
    Integer(i64),
    Bool(bool),

    //operators
    Assign,
    Plus,
    Minus,
    Asterisk,
    Slash,

    Bang,
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,

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
    If,
    Else,
    Return,
}
