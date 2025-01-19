#![cfg(test)]

use super::{ParsingError, Token};
use std::iter::Peekable;
use std::fmt::{Debug, Display};
use std::cmp::Eq;

type ParsingTestExpected<T> = Result<T, ParsingError>;

#[derive(Clone)]
pub struct ParsingTest<T: Clone> {
    pub test_name: &'static str,
    pub tokens: Vec<Token>,
    pub expected: ParsingTestExpected<T>,
    pub next_token: Option<Token>,
}

type VecTokenIntoIter = <Vec<Token> as IntoIterator>::IntoIter;

pub fn test_parser<T: Debug + Display + Eq + Clone>(
    tests: &[ParsingTest<T>],
    parser: impl Fn(&mut Peekable<VecTokenIntoIter>) -> ParsingTestExpected<T>
) {
    for ParsingTest { test_name, tokens, expected, next_token } in tests {
        let mut iter = tokens.clone().into_iter().peekable();
        let got = parser(&mut iter);
        let iter_next = iter.next();

        assert!(got == *expected, "{}", assert_message_parsing_failed(test_name, tokens, &got, expected));
        assert!(iter_next == *next_token, "{}", assert_message_wrong_next_token(test_name, tokens, &iter_next, next_token));
    }
}

fn format_parsing_result<T: Display>(result: &ParsingTestExpected<T>) -> String {
    match result {
        Ok(ok) => format!("{}", ok),
        Err(err) => format!("{}", err)
    }
}
fn format_token_vec(tokens: &[Token]) -> String {
    let mut out = "".to_string();
    for token in tokens {
        out += &format!(" {token}");
    }
    return out.trim().to_string()
}

fn assert_message_parsing_failed<T: Display>(
    test_name: &str,
    tokens: &[Token],
    got: &ParsingTestExpected<T>,
    expected: &ParsingTestExpected<T>
) -> String {
        format!("Assertion failed on test \"{}\":\n   tokens:   {}\n   got:      {}\n   expected: {}\n",
            test_name,
            format_token_vec(tokens),
            format_parsing_result(got),
            format_parsing_result(expected)
        )
}

fn assert_message_wrong_next_token(
    test_name: &str,
    tokens: &[Token],
    next_token: &Option<Token>,
    expected_next_token: &Option<Token>
) -> String {
        format!("Assertion failed on test \"{}\":\n   tokens:   {}\n   got:      {:?}\n   expected: {:?}\n",
            test_name,
            format_token_vec(tokens),
            next_token,
            expected_next_token
        )
}
