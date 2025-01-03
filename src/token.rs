use std::fmt::{Display, Formatter};

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

impl Display for Token {
    fn fmt (&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Token::Illegal(string) => format!("illegal token ({})", string),

            Token::Identifier(string) => {
                if string.len() == 0 {
                    "an identifier".to_string()
                } else {
                    format!("identifier (\"{}\")", string)
                }
            },

            //literals
            Token::Integer(_integer) => "integer".to_string(), //TODO: make these display the value
            Token::Bool(_bool) => "boolean".to_string(),

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
            // TODO: convert Identifier(String) with special value to Identifier(Option<String>)
            // (18 compile errors)
            Test {
                input: Token::Identifier("foobar".to_string()),
                expected: "identifier (\"foobar\")".to_string(),
            },
            Test {
                input: Token::Identifier("".to_string()),
                expected: "an identifier".to_string(),
            },
            //integer
            // TODO: convert Integer(u64) to Integer(Option<u64>)
            // (28 compile errors)
            Test {
                input: Token::Integer(12),
                expected: "integer".to_string(),
                //expected: "integer literal (12)".to_string(),
            },
            Test {
                input: Token::Integer(-3),
                expected: "integer".to_string(),
                //expected: "integer literal (-3)".to_string(),
            },
            /*
            Test {
                input: Token::Integer(None),
                expected: "an integer literal".to_string(),
            },
            */
            //bool
            // TODO: convert Bool(bool) to Bool(Option<bool>)
            // (6 compile errors)
            Test {
                input: Token::Bool(true),
                expected: "boolean".to_string(),
                //expected: "boolean literal (true)".to_string(),
            },
            /*
            Test {
                input: Token::Bool(None),
                expected: "a boolean literal".to_string()
            }
            */

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
