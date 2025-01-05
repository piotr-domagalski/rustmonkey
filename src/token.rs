use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    Illegal(String),

    Identifier(String),

    //literals
    Integer(i64),
    Bool(bool),

    //marker variants for errors
    AnIdentifier,
    AnInteger,
    ABool,

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

impl Token {
    pub fn new_ident(ident: &str) -> Token {
        Token::Identifier(ident.to_string())
    }
    pub fn new_int(int: i64) -> Token {
        Token::Integer(int)
    }
    pub fn new_bool(b: bool) -> Token {
        Token::Bool(b)
    }

}

impl Display for Token {
    fn fmt (&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Token::Illegal(string) => format!("illegal token ({})", string),

            Token::Identifier(string) => {
                format!("identifier (\"{}\")", string)
            },
            Token::AnIdentifier => {
                "an identifier".to_string()
            }

            //literals
            Token::Integer(int) => format!("integer ({})", int),
            Token::AnInteger => "an integer".to_string(),
            Token::Bool(bool) => format!("boolean ({})", bool),
            Token::ABool => "a boolean".to_string(),

            //operators
            Token::Assign => "=".to_string(),
            Token::Plus => "+".to_string(),
            Token::Minus => "-".to_string(),
            Token::Asterisk => "*".to_string(),
            Token::Slash => "/".to_string(),

            Token::Bang => "!".to_string(),
            Token::Equals => "==".to_string(),
            Token::NotEquals => "!=".to_string(),
            Token::LessThan => "<".to_string(),
            Token::GreaterThan => ">".to_string(),

            //separators
            Token::Comma => ",".to_string(),
            Token::Semicolon => ";".to_string(),

            //brackets
            Token::LeftRound => "(".to_string(),
            Token::RightRound => ")".to_string(),
            Token::LeftSquare => "[".to_string(),
            Token::RightSquare => "]".to_string(),
            Token::LeftCurly => "{".to_string(),
            Token::RightCurly => "}".to_string(),

            //keywords
            Token::Function => "fn".to_string(),
            Token::Let => "let".to_string(),
            Token::If => "if".to_string(),
            Token::Else => "else".to_string(),
            Token::Return => "return".to_string(),
        };
        write!(f, "{}", str)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_token_display() {
        struct Test {
            input: Token,
            expected: String,
        }

        let tests = [
            //illegal
           Test {
                input: Token::Illegal("message1".to_string()),
                expected: "illegal token (message1)".to_string(),
            },

            //identifier
            Test {
                input: Token::new_ident("foobar"),
                expected: "identifier (\"foobar\")".to_string(),
            },
            Test {
                input: Token::AnIdentifier,
                expected: "an identifier".to_string(),
            },

            //integer
            Test {
                input: Token::new_int(12),
                expected: "integer (12)".to_string(),
            },
            Test {
                input: Token::new_int(-3),
                expected: "integer (-3)".to_string(),
            },
            Test {
                input: Token::AnInteger,
                expected: "an integer".to_string(),
            },

            Test {
                input: Token::new_bool(true),
                expected: "boolean (true)".to_string(),
            },
            Test {
                input: Token::ABool,
                expected: "a boolean".to_string()
            },

            //some others
            Test {
                input: Token::Bang,
                expected: "!".to_string(),
            },
            Test {
                input: Token::NotEquals,
                expected: "!=".to_string(),
            },
            Test {
                input: Token::LeftRound,
                expected: "(".to_string(),
            },
        ];

        for Test{input, expected} in tests {
            assert_eq!(input.to_string(), expected);
        }
    }
}
