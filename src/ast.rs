use crate::token::Token;

pub trait TokenIter: Iterator<Item = Token> {}
impl<T: Iterator<Item = Token>> TokenIter for T {}

macro_rules! next_if_eq_else_return_err {
    ( $iter:ident, $tok:expr, $parsing_what:expr, unexpected ) => {
        if $iter.next_if_eq( &$tok ).is_none() { return Err(ParsingError::new_unexpected($iter.peek(), vec![$tok], $parsing_what)); }
    };
    ( $iter:ident, $tok:expr, $parsing_what:expr, other, $message:expr ) => {
        if $iter.next_if_eq( &$tok ).is_none() { return Err(ParsingError::new_other($message)); }
    };
}

macro_rules! peek_if_eq_else_return_err {
    ( $iter:ident, $tok:expr, $parsing_what:expr, unexpected ) => {
        if $iter.peek() != Some(&$tok ) { return Err(ParsingError::new_unexpected($iter.peek(), vec![$tok], $parsing_what)); }
    };
    ( $iter:ident, $tok:expr, $parsing_what:expr, other, $message: expr ) => {
        if $iter.peek() != Some(&$tok ) { return Err(ParsingError::new_other($message)); }
    };
}

mod program;
mod statement;
mod expression;
mod parsing_error;

#[allow(unused_imports, reason="program::* will remain unused until source file interpretation is implemented")]
pub use program::*;
pub use statement::*;
pub use expression::*;
pub use parsing_error::*;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::Token;
    use crate::lexer::Lexer;

    #[test]
    fn test_token_iter_trait() {
        let code = "let x = 5;";
        let tokens = vec![Token::Let, Token::Identifier(String::from("x")), Token::Assign, Token::Integer(5), Token::Semicolon];

        let lexer = Lexer::new(code);
        let parsed_from_lexer = Program::parse(&mut lexer.peekable());
        let parsed_from_tokens = Program::parse(&mut tokens.into_iter().peekable());

        assert_eq!(parsed_from_tokens, parsed_from_lexer);
    }
}
