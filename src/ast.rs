#![allow(unused)]

use crate::token::Token;

pub trait TokenIter: Iterator<Item = Token> {}
impl<T: Iterator<Item = Token>> TokenIter for T {}

mod program;
mod statement;
mod expression;
mod parsing_error;

pub use program::*;
pub use statement::*;
pub use expression::*;
pub use parsing_error::*;
