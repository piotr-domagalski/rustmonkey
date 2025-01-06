use crate::token::Token;
use std::fmt::{Display, Formatter};

#[derive(PartialEq, Eq, Debug)]
pub enum ParsingError {
    UnexpectedToken {
        got: Option<Token>,
        expected: Vec<Token>,
        parsing_what: String,
    },
    MultipleErrors {
        errors: Vec<ParsingError>,
        parsing_what: String,
    },
    OtherError {
        message: String,
        parsing_what: String,
    }
}

//builders
impl ParsingError {
    pub fn new_unexpected(got: Option<&Token>, expected: Vec<Token>, parsing_what: &str) -> ParsingError {
        let got = match got {
            None => None,
            Some(tok) => Some(tok.clone())
        };
        ParsingError::UnexpectedToken { got, expected, parsing_what: parsing_what.to_string()}
    }
    pub fn new_multiple(errors: Vec<ParsingError>, parsing_what: &str) -> ParsingError {
        ParsingError::MultipleErrors { errors, parsing_what: parsing_what.to_string() }
    }
    pub fn new_other(message: &str, parsing_what: &str) -> ParsingError {
        ParsingError::OtherError { message: message.to_string(), parsing_what: parsing_what.to_string() }
    }
}

//conversions
impl From<&str> for ParsingError {
    fn from(message: &str) -> Self {
        ParsingError::new_other(message, "unknown")
    }
}
impl From<Vec<&str>> for ParsingError {
    fn from(errors: Vec<&str>) -> Self {
        let mut output: Vec<ParsingError> = vec![];
        for error in errors {
            output.push(error.into());
        }
        ParsingError::new_multiple(output, "unknown")
    }
}
impl From<ParsingError> for &str {
    fn from(value: ParsingError) -> Self {
        value.to_string().leak()
    }
}

//display helpers
impl ParsingError {
    fn display_option_token( option_token: &Option<Token>) -> String {
        match option_token {
            Some(got) => {
                got.to_string()
            }
            None =>
                "None".to_string()
        }
    }
    fn display_vec_token( vec_token: &Vec<Token>) -> String {
        match vec_token.len() {
            0 => "None".to_string(),
            1 => vec_token[0].to_string(),
            _ => {
                vec_token.iter()
                    .map(|tok| tok.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            }
        }
    }
}

impl Display for ParsingError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedToken { got, expected, parsing_what } => {
                let got = Self::display_option_token(&got);
                let expected = Self::display_vec_token(&expected);
                write!(f, "Unexpected token while parsing {parsing_what} - got: {got}, expected: {expected}")
            },
            Self::MultipleErrors { errors, parsing_what } => {
                write!(f, "Multiple errors while parsing {parsing_what} - len={}", errors.len())?;
                for error in errors {
                    write!(f, "\n{}", error)?;
                }
                Ok(())
            },
            Self::OtherError { message, parsing_what } => {
                if message.len() == 0 {
                    write!(f, "Other error while parsing {parsing_what}")
                } else {
                    write!(f, "Other error while parsing {parsing_what}: {message}")
                }
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display_option_token() {
        struct Test {
            input: Option<Token>,
            expected: String
        }
        let tests = [
            Test {
                input: Option::Some(Token::Bang),
                expected: "!".to_string(),
            },
            Test {
                input: Option::None,
                expected: "None".to_string(),
            }
        ];

        for Test{input, expected} in tests {
            assert_eq!(ParsingError::display_option_token(&input), expected);
        }
    }

    #[test]
    fn test_display_vec_token() {
        struct Test {
            input: Vec<Token>,
            expected: String
        }

        let tests = [
            Test {
                input: vec![Token::Bang, Token::Plus, Token::Function],
                expected: format!("{}, {}, {}", Token::Bang, Token::Plus, Token::Function),
            },
            Test {
                input: vec![Token::Equals],
                expected: Token::Equals.to_string(),
            },
            Test {
                input: vec![],
                expected: "None".to_string(),
            }
        ];

        for Test{input, expected} in tests {
            assert_eq!(ParsingError::display_vec_token(&input), expected);
        }
    }

    #[test]
    fn test_display_parsing_error() {
        struct Test {
            input: ParsingError,
            expected: String,
        }

        let tests = [
            //unexpected: got - None and Some, expected - empty, 1, 3
            Test {
                input: ParsingError::new_unexpected(
                    None,
                    vec![Token::Semicolon],
                    "test1"),
                expected: format!("Unexpected token while parsing {} - got: {}, expected: {}",
                    "test1",
                    ParsingError::display_option_token(&None),
                    ParsingError::display_vec_token(&vec![Token::Semicolon])),
            },
            Test {
                input: ParsingError::new_unexpected(
                    Some(&Token::new_ident("barbaz")),
                    vec![Token::Comma, Token::Semicolon],
                    "test2"),
                expected: format!("Unexpected token while parsing {} - got: {}, expected: {}",
                    "test2",
                    ParsingError::display_option_token(&Some(Token::new_ident("barbaz"))),
                    ParsingError::display_vec_token(&vec![Token::Comma, Token::Semicolon])),
            },
            Test {
                input: ParsingError::new_unexpected(
                    Some(&Token::Bang),
                    vec![],
                    "test3"),
                expected: format!("Unexpected token while parsing {} - got: {}, expected: {}",
                    "test3",
                    ParsingError::display_option_token(&Some(Token::Bang)),
                    ParsingError::display_vec_token(&vec![])),
            },

            //multiple errors
            Test {
                input: ParsingError::new_multiple(vec![], "test4"),
                expected: format!("Multiple errors while parsing test4 - len={}", 0),
            },
            Test {
                input: ParsingError::new_multiple(vec![
                        ParsingError::new_unexpected(Some(&Token::Bang), vec![Token::Equals, Token::NotEquals], "test5"),
                    ],
                    "test5"
                ),
                expected: format!("Multiple errors while parsing test5 - len={}\n{}",
                    1,
                    ParsingError::new_unexpected(Some(&Token::Bang), vec![Token::Equals, Token::NotEquals], "test5")),
            },
            Test {
                input: ParsingError::new_multiple(vec![
                        ParsingError::new_other("some other error in test6a", "test6a"),
                        ParsingError::new_unexpected(Some(&Token::LeftCurly), vec![Token::RightCurly], "test6b"),
                    ],
                    "test6"
                ),
                expected: format!("Multiple errors while parsing test6 - len={}\n{}\n{}",
                    2,
                    ParsingError::new_other("some other error in test6a", "test6a"),
                    ParsingError::new_unexpected(Some(&Token::LeftCurly), vec![Token::RightCurly], "test6b")),
            },

            //other
            Test {
                input: ParsingError::new_other("", "test7"),
                expected: "Other error while parsing test7".to_string(),
            },
            Test {
                input: ParsingError::new_other("some other error in test8", "test8"),
                expected: "Other error while parsing test8: some other error in test8".to_string(),
            }
        ];

        for Test{input, expected} in tests {
            assert_eq!(input.to_string(), expected);
        }
    }
}
