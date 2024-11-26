use crate::token::Token;

#[derive(PartialEq, Eq, Debug)]
pub enum ParsingError {
    UnexpectedToken {
        got: Option<Token>,
        expected: Vec<Token>,
        parsing_what: String,
    },
    MultipleErrors {
        errors: Vec<ParsingError>,
    },
    OtherError {
        message: String
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
    pub fn new_multiple(errors: Vec<ParsingError>) -> ParsingError {
        ParsingError::MultipleErrors { errors }

    }

    pub fn new_other(message: &str) -> ParsingError {
        ParsingError::OtherError { message: message.to_string() }
    }
}

//conversions
impl From<&str> for ParsingError {
    fn from(message: &str) -> Self {
        ParsingError::new_other(message)
    }

}
impl From<Vec<&str>> for ParsingError {
    fn from(errors: Vec<&str>) -> Self {
        let mut output: Vec<ParsingError> = vec![];
        for error in errors {
            output.push(error.into());
        }
        ParsingError::new_multiple(output)
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
                    .map(|tok| {let mut newtok = tok.to_string(); newtok})
                    .collect::<Vec<String>>()
                    .join(", ")
            }
        }
    }
}

use std::fmt::{Display, Formatter};
impl Display for ParsingError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedToken { got, expected, parsing_what } => {
                let got = Self::display_option_token(&got);
                let expected = Self::display_vec_token(&expected);
                write!(f, "Unexpected token while parsing {parsing_what} - got: {got}, expected: {expected}")
            },
            Self::MultipleErrors { errors } => {
                write!(f, "Multiple errors - len={}", errors.len())?;
                for error in errors {
                    write!(f, "\n{}", error)?;
                }
                Ok(())
            },
            Self::OtherError { message } => {
                if message.len() == 0 {
                    write!(f, "Other error")
                } else {
                    write!(f, "Other error: {message}")
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
        };
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
        };

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
        };

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
                    Some(&Token::Identifier("barbaz".to_string())),
                    vec![Token::Comma, Token::Semicolon],
                    "test2"),
                expected: format!("Unexpected token while parsing {} - got: {}, expected: {}",
                    "test2",
                    ParsingError::display_option_token(&Some(Token::Identifier("barbaz".to_string()))),
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
                input: ParsingError::new_multiple(vec![]),
                expected: format!("Multiple errors - len={}", 0),
            },
            Test {
                input: ParsingError::new_multiple(vec![
                    ParsingError::new_unexpected(Some(&Token::Bang), vec![Token::Equals, Token::NotEquals], "test5"),
                ]),
                expected: format!(r#"Multiple errors - len={}
{}"#,
                    1,
                    ParsingError::new_unexpected(Some(&Token::Bang), vec![Token::Equals, Token::NotEquals], "test5")),
            },
            Test {
                input: ParsingError::new_multiple(vec![
                    ParsingError::new_other("some other error in test6a"),
                    ParsingError::new_unexpected(Some(&Token::LeftCurly), vec![Token::RightCurly], "test6b"),
                ]),
                expected: format!("Multiple errors - len={}
{}
{}",
                    2,
                    ParsingError::new_other("some other error in test6a"),
                    ParsingError::new_unexpected(Some(&Token::LeftCurly), vec![Token::RightCurly], "test6b")),
            },

            //other
            Test {
                input: ParsingError::new_other(""),
                expected: "Other error".to_string(),
            },
            Test {
                input: ParsingError::new_other("some other error in test8"),
                expected: "Other error: some other error in test8".to_string(),
            }
        ];

        for Test{input, expected} in tests {
            assert_eq!(input.to_string(), expected);
        }
    }
}
/*
 --------------------- expected something specific ---------------
--
ast/statement.rs-53-    fn parse_let_statement<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Statement, &'static str> {
ast/statement.rs:54:        if iter.next_if_eq(&Token::Let).is_none() { return Err("let keyword expected"); };
ast/statement.rs-55-        let identifier = IdentifierExpression::parse(iter)?;
ast/statement.rs:56:        if iter.next_if_eq(&Token::Assign).is_none() { return Err("assignment operator expected"); };
ast/statement.rs-57-        let expression = Expression::parse(iter)?;
ast/statement.rs:58:        if iter.next_if_eq(&Token::Semicolon).is_none() { return Err("semicolon expected")};
ast/statement.rs-59-    
--
ast/statement.rs-64-    {
ast/statement.rs:65:        if iter.next_if_eq(&Token::Return).is_none() { return Err("return keyword expected"); };
ast/statement.rs-66-        let expression = Expression::parse(iter)?;
ast/statement.rs:67:        if iter.next_if_eq(&Token::Semicolon).is_none() { return Err("semicolon expected")};
ast/statement.rs-68-
--
ast/expression/operators.rs-17-            Token::Bang => Ok(Negation),
ast/expression/operators.rs:18:            _ => Err("Expected ! or - operator"),
ast/expression/operators.rs-19-        }
--
--
ast/expression.rs-128-            },
ast/expression.rs:129:            _ => Err("expected identifier token"),
ast/expression.rs-130-        }
--
ast/expression.rs-168-            _ => 
ast/expression.rs:169:                Err("expected integer literal token"),
ast/expression.rs-170-        }

 --------------------- other ---------------
ast/statement.rs-43-        match iter.peek() {
ast/statement.rs:44:            None => Err("EOF"),
ast/statement.rs-45-            Some(Token::Let) => Self::parse_let_statement(iter),
--
ast/statement.rs-47-            _ => Self::parse_expression_statement(iter),
ast/statement.rs:48:            // TODO: Should this be a wildcard? should there be an Err("unimplemented statement type")?
ast/statement.rs-49-        }


 -------- expected many options -------
ast/expression/operators.rs-70-            Token::NotEquals => Ok(NotEquals),
ast/expression/operators.rs:71:            _ => Err("invalid operator"),
ast/expression/operators.rs-72-        }
--
ast/expression.rs-53-            Some(Token::Bang) | Some(Token::Minus) => Expression::parse_prefix_expression(iter)?,
ast/expression.rs:54:            _ => return Err("unimplemented expression type")
ast/expression.rs-55-        };

------- multiple errors ------
ast/program.rs-36-        else {
ast/program.rs:37:            Err(errors)
ast/program.rs-38-        }

 --------------------- will go away after cleanup ---------------
--
ast/expression.rs-78-            Some(token) => PrefixOperator::parse(&token)?,
ast/expression.rs:79:            None => return Err("unexpected EOF"),
ast/expression.rs-80-        };

--
ast/expression.rs-91-        } else {
ast/expression.rs:92:            Err("expected operator token")
ast/expression.rs-93-        } 

 --------------------- not parsingerrors ---------------
--
ast/expression.rs-62-                        Ok(op) => op.precedence(),
ast/expression.rs:63:                        Err(_) => Precedence::Lowest,
ast/expression.rs-64-                    }
--
ast/program.rs-27-                Ok(statement) => statements.push(statement),
ast/program.rs:28:                Err("EOF") => break,
ast/program.rs:29:                Err(error) => errors.push(error),
ast/program.rs-30-            }
*/
