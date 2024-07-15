use crate::token::*;

use std::str::Chars;
use std::iter::Peekable;

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>
}

impl Iterator for Lexer<'_> {
    type Item = Token;
    
    fn next(&mut self) -> Option<Self::Item>{
        while self.input.next_if(|ch| ch.is_whitespace()).is_some() {}

        if let Some(ch) = self.input.next() {
            match ch {
                '=' => { match self.input.peek() {
                        Some('=') => {self.input.next(); Some(Token::Equals)},
                        _ => Some(Token::Assign), } },
                '+' => Some(Token::Plus),
                '-' => Some(Token::Minus),
                '*' => Some(Token::Asterisk),
                '/' => Some(Token::Slash),

                '!' => { match self.input.peek() {
                        Some('=') => {self.input.next(); Some(Token::NotEquals)},
                        _ => Some(Token::Bang), } },
                '<' => Some(Token::LessThan),
                '>' => Some(Token::GreaterThan),
                ',' => Some(Token::Comma),
                ';' => Some(Token::Semicolon),
                '(' => Some(Token::LeftRound),
                ')' => Some(Token::RightRound),
                '{' => Some(Token::LeftCurly),
                '}' => Some(Token::RightCurly),
                '[' => Some(Token::LeftSquare),
                ']' => Some(Token::RightSquare),
                ch if ch.is_numeric() => {
                    let remaining_chars = std::iter::from_fn(
                        || self.input.next_if(|ch| ch.is_numeric())
                        );
                    let string = std::iter::once(ch)
                        .chain(remaining_chars)
                        .collect::<String>();
                    if let Ok(int) = string.parse::<i64>() {
                        Some(Token::Integer(int))
                    }
                    else {
                        Some(Token::Illegal(string))
                    }
                },
                ch if ch.is_alphabetic() => {
                    let remaining_chars = std::iter::from_fn(
                        || self.input.next_if(|ch| ch.is_alphanumeric() || *ch == '_')
                        );
                    let word = std::iter::once(ch)
                        .chain(remaining_chars)
                        .collect::<String>();
                    match word.as_str() {
                        "let" => Some(Token::Let),
                        "fn" => Some(Token::Function),
                        "if" => Some(Token::If),
                        "else" => Some(Token::Else),
                        "return" => Some(Token::Return),
                        "true" => Some(Token::Bool(true)),
                        "false" => Some(Token::Bool(false)),
                        _ => Some(Token::Identifier(word)),
                    }
                }
                other => Some(Token::Illegal(String::from(other))),
            }
        }
        else {
            None
        }
    }

}

impl<'a> Lexer<'a> {
    pub fn new(s: &str) -> Lexer {
        Lexer {
            input: s.chars().peekable(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_specials() {
        let input = "=+,;(){}[]";

        let mut lexer = Lexer::new(input);

        let output = [
            Some(Token::Assign),
            Some(Token::Plus),
            Some(Token::Comma),
            Some(Token::Semicolon),
            Some(Token::LeftRound),
            Some(Token::RightRound),
            Some(Token::LeftCurly),
            Some(Token::RightCurly),
            Some(Token::LeftSquare),
            Some(Token::RightSquare),
            None,
        ];

        for expected in output.into_iter() {
            assert_eq!(lexer.next(), expected);
        }
    }

    #[test]
    fn test_code_basic() {
        let input = "let five = 5;
let ten = 10;
let add = fn(x, y) {
x + y;
};
let result = add(five, ten);";
        
        let mut lexer = Lexer::new(input);

        let output = [
            Some(Token::Let),
            Some(Token::Identifier(String::from("five"))),
            Some(Token::Assign),
            Some(Token::Integer(5)),
            Some(Token::Semicolon),
            Some(Token::Let),
            Some(Token::Identifier(String::from("ten"))),
            Some(Token::Assign),
            Some(Token::Integer(10)),
            Some(Token::Semicolon),
            Some(Token::Let),
            Some(Token::Identifier(String::from("add"))),
            Some(Token::Assign),
            Some(Token::Function),
            Some(Token::LeftRound),
            Some(Token::Identifier(String::from("x"))),
            Some(Token::Comma),
            Some(Token::Identifier(String::from("y"))),
            Some(Token::RightRound),
            Some(Token::LeftCurly),
            Some(Token::Identifier(String::from("x"))),
            Some(Token::Plus),
            Some(Token::Identifier(String::from("y"))),
            Some(Token::Semicolon),
            Some(Token::RightCurly),
            Some(Token::Semicolon),
            Some(Token::Let),
            Some(Token::Identifier(String::from("result"))),
            Some(Token::Assign),
            Some(Token::Identifier(String::from("add"))),
            Some(Token::LeftRound),
            Some(Token::Identifier(String::from("five"))),
            Some(Token::Comma),
            Some(Token::Identifier(String::from("ten"))),
            Some(Token::RightRound),
            Some(Token::Semicolon),
            None,
        ];

        for expected in output {
            assert_eq!(lexer.next(), expected);
        }
    }

    #[test]
    fn test_code_remaining() {
        let input = "!-/*5;
5 < 10 > 5;
if (5 < 10) {
return true;
} else {
return false;
}
10 == 10;
10 != 9;";
        
        let mut lexer = Lexer::new(input);

        let output = [
            Some(Token::Bang),
            Some(Token::Minus),
            Some(Token::Slash),
            Some(Token::Asterisk),
            Some(Token::Integer(5)),
            Some(Token::Semicolon),

            Some(Token::Integer(5)),
            Some(Token::LessThan),
            Some(Token::Integer(10)),
            Some(Token::GreaterThan),
            Some(Token::Integer(5)),
            Some(Token::Semicolon),

            Some(Token::If),
            Some(Token::LeftRound),
            Some(Token::Integer(5)),
            Some(Token::LessThan),
            Some(Token::Integer(10)),
            Some(Token::RightRound),
            Some(Token::LeftCurly),

            Some(Token::Return),
            Some(Token::Bool(true)),
            Some(Token::Semicolon),

            Some(Token::RightCurly),
            Some(Token::Else),
            Some(Token::LeftCurly),

            Some(Token::Return),
            Some(Token::Bool(false)),
            Some(Token::Semicolon),

            Some(Token::RightCurly),

            Some(Token::Integer(10)),
            Some(Token::Equals),
            Some(Token::Integer(10)),
            Some(Token::Semicolon),

            Some(Token::Integer(10)),
            Some(Token::NotEquals),
            Some(Token::Integer(9)),
            Some(Token::Semicolon),

            None,
        ];

        for expected in output {
            assert_eq!(lexer.next(), expected);
        }
    }
}
