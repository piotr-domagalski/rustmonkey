mod token;
mod lexer;
mod parsing_error;
pub mod testing_common;

pub use token::Token;
pub use lexer::Lexer;
pub use parsing_error::ParsingError;

macro_rules! next_if_eq_else_return_err {
    ( $iter:ident, $tok:expr, $parsing_what:expr, unexpected ) => {
        if $iter.next_if_eq( &$tok ).is_none() { return Err(ParsingError::new_unexpected($iter.peek(), vec![$tok], $parsing_what)); }
    };
    ( $iter:ident, $tok:expr, $parsing_what:expr, other, $message:expr ) => {
        if $iter.next_if_eq( &$tok ).is_none() { return Err(ParsingError::new_other($message, $parsing_what)); }
    };
}

macro_rules! peek_if_eq_else_return_err {
    ( $iter:ident, $tok:expr, $parsing_what:expr, unexpected ) => {
        if $iter.peek() != Some(&$tok ) { return Err(ParsingError::new_unexpected($iter.peek(), vec![$tok], $parsing_what)); }
    };
    ( $iter:ident, $tok:expr, $parsing_what:expr, other, $message: expr ) => {
        if $iter.peek() != Some(&$tok ) { return Err(ParsingError::new_other($message, $parsing_what)); }
    };
}

pub(crate) use next_if_eq_else_return_err;
pub(crate) use peek_if_eq_else_return_err;

pub trait TokenIter: Iterator<Item = Token> {}
impl<T: Iterator<Item = Token>> TokenIter for T {}

pub trait Parsable: Sized {
    fn parse<I: TokenIter>(iter: &mut std::iter::Peekable<I>) -> Result<Self, ParsingError>;
}

#[cfg(test)]
mod tests {
    use crate::parsing::Token;
    use crate::parsing::Lexer;
    use super::Parsable;

    #[test]
    fn test_token_iter_trait() {
        let code = "let x = 5;";
        let tokens = vec![Token::Let, Token::new_ident("x"), Token::Assign, Token::new_int(5), Token::Semicolon];

        let lexer = Lexer::new(code);
        let parsed_from_lexer = crate::ast::Program::parse(&mut lexer.peekable());
        let parsed_from_tokens = crate::ast::Program::parse(&mut tokens.into_iter().peekable());

        assert_eq!(parsed_from_tokens, parsed_from_lexer);
    }
}
