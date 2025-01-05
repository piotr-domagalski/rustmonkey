use crate::token::*;
use std::str::Chars;
use std::iter::Peekable;

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>
}

impl<'a> Lexer<'a> {
    pub fn new(s: &str) -> Lexer {
        Lexer {
            input: s.chars().peekable(),
        }
    }

    fn eat_whitespace(&mut self) {
        while self.input.next_if(|ch| ch.is_whitespace()).is_some() {};
    }

    fn collect_matching_to_string<F>(&mut self, ch: char, mut p: F) -> String
    where
        F: FnMut(&char) -> bool
    {
        std::iter::once(ch)
            .chain(std::iter::from_fn(
                || self.input.next_if(|ch| p(ch))))
            .collect::<String>()
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item>{
        self.eat_whitespace();

        if let Some(ch) = self.input.next() {
            Some(match ch {
                '=' if Some(&'=') == self.input.peek()
                    => { self.input.next(); Token::Equals },
                '=' => Token::Assign,
                '+' => Token::Plus,
                '-' => Token::Minus,
                '*' => Token::Asterisk,
                '/' => Token::Slash,

                '!' if Some(&'=') == self.input.peek()
                    => { self.input.next(); Token::NotEquals },
                '!' => Token::Bang,
                '<' => Token::LessThan,
                '>' => Token::GreaterThan,

                ',' => Token::Comma,
                ';' => Token::Semicolon,

                '(' => Token::LeftRound,
                ')' => Token::RightRound,
                '{' => Token::LeftCurly,
                '}' => Token::RightCurly,
                '[' => Token::LeftSquare,
                ']' => Token::RightSquare,

                ch if ch.is_numeric() => {
                    let string = self.collect_matching_to_string(
                        ch, |ch| ch.is_numeric()
                        );
                    match string.parse::<i64>() {
                        Ok(int) => Token::Integer(int),
                        _ => Token::Illegal(string),
                    }
                },
                ch if ch.is_alphabetic() => {
                    let string = self.collect_matching_to_string(
                        ch, |ch| ch.is_alphanumeric() || *ch == '_'
                        );
                    match string.as_str() {
                        "let" => Token::Let,
                        "fn" => Token::Function,
                        "if" => Token::If,
                        "else" => Token::Else,
                        "return" => Token::Return,
                        "true" => Token::new_bool(true),
                        "false" => Token::new_bool(false),
                        _ => Token::Identifier(string),
                    }
                },

                other => Token::Illegal(String::from(other)),
            })
        } else {
            None
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
            Some(Token::Let), Some(Token::new_ident("five")), Some(Token::Assign), Some(Token::new_int(5)), Some(Token::Semicolon),
            Some(Token::Let), Some(Token::new_ident("ten")), Some(Token::Assign), Some(Token::new_int(10)), Some(Token::Semicolon),
            Some(Token::Let), Some(Token::new_ident("add")), Some(Token::Assign), Some(Token::Function),
            Some(Token::LeftRound),
                Some(Token::new_ident("x")), Some(Token::Comma),
                Some(Token::new_ident("y")),
            Some(Token::RightRound),
            Some(Token::LeftCurly),
                Some(Token::new_ident("x")), Some(Token::Plus), Some(Token::new_ident("y")), Some(Token::Semicolon),
            Some(Token::RightCurly), Some(Token::Semicolon),

            Some(Token::Let), Some(Token::new_ident("result")), Some(Token::Assign),
            Some(Token::new_ident("add")), Some(Token::LeftRound),
                Some(Token::new_ident("five")), Some(Token::Comma),
                Some(Token::new_ident("ten")),
            Some(Token::RightRound), Some(Token::Semicolon),
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
            Some(Token::new_int(5)),
            Some(Token::Semicolon),

            Some(Token::new_int(5)),
            Some(Token::LessThan),
            Some(Token::new_int(10)),
            Some(Token::GreaterThan),
            Some(Token::new_int(5)),
            Some(Token::Semicolon),

            Some(Token::If), Some(Token::LeftRound),
                Some(Token::new_int(5)), Some(Token::LessThan), Some(Token::new_int(10)),
            Some(Token::RightRound), Some(Token::LeftCurly),
                Some(Token::Return), Some(Token::new_bool(true)), Some(Token::Semicolon),
            Some(Token::RightCurly), Some(Token::Else), Some(Token::LeftCurly),
                Some(Token::Return), Some(Token::new_bool(false)), Some(Token::Semicolon),
            Some(Token::RightCurly),

            Some(Token::new_int(10)), Some(Token::Equals), Some(Token::new_int(10)), Some(Token::Semicolon),
            Some(Token::new_int(10)), Some(Token::NotEquals), Some(Token::new_int(9)), Some(Token::Semicolon),

            None,
        ];

        for expected in output {
            assert_eq!(lexer.next(), expected);
        }
    }
}
