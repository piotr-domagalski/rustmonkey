use crate::token::Token;

trait TokenIter: Iterator<Item = Token> {}
impl<T: Iterator<Item = Token>> TokenIter for T {}

mod program;
mod statement;
mod expression;

pub use program::*;
pub use statement::*;
pub use expression::*;
