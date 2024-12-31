#![allow(unused)]
use crate::token::Token;
use crate::ast::TokenIter;
use crate::ast::Expression;
use crate::ast::IdentifierExpression;
use crate::ast::ParsingError;
use std::iter::Peekable;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    Let{ identifier: IdentifierExpression, expression: Expression},
    Return { expression: Expression},
    Expression {expression: Expression},
    Block { body: BlockStatement },
    /*
    IfElse(IfElseStatement),
    Empty,
    None,
    */
}
//builders
impl Statement {
    pub fn new_let(identifier: IdentifierExpression, expression: Expression) -> Statement{
        Statement::Let{
            identifier,
            expression,
        }
    }
    pub fn new_return(expression: Expression) -> Statement {
        Statement::Return {
            expression
        }
    }

    pub fn new_expr(expression: Expression) -> Statement {
        Statement::Expression {
            expression
        }
    }
    pub fn new_block(block_statement: BlockStatement) -> Statement {
        Statement::Block {
            body: block_statement
        }
    }
}
//parsing
impl Statement {
    pub fn parse<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Statement, ParsingError>
    {
        match iter.peek() {
            None => Err(ParsingError::new_other("EOF")),
            Some(Token::Let) => Self::parse_let_statement(iter),
            Some(Token::Return) => Self::parse_return_statement(iter),
            Some(Token::LeftCurly) => Self::parse_block_statement(iter),
            _ => Self::parse_expression_statement(iter),
        }
    }

    // let <identifier> = <expression> ;
    fn parse_let_statement<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Statement, ParsingError> {
        if iter.next_if_eq(&Token::Let).is_none() { return Err(ParsingError::new_unexpected(iter.peek(), vec![Token::Let], "let statement")); }
        let identifier = IdentifierExpression::parse(iter)?;
        if iter.next_if_eq(&Token::Assign).is_none() { return Err(ParsingError::new_unexpected(iter.peek(), vec![Token::Assign], "let statement")); }
        let expression = Expression::parse(iter)?;
        if iter.next_if_eq(&Token::Semicolon).is_none() { return Err(ParsingError::new_unexpected(iter.peek(), vec![Token::Semicolon], "let statement")); }
    
        Ok(Statement::new_let(identifier, expression))
    }

    // return <expression> ;
    fn parse_return_statement<I: TokenIter> (iter: &mut Peekable<I>) -> Result<Statement, ParsingError>
    {
        if iter.next_if_eq(&Token::Return).is_none() { return Err(ParsingError::new_unexpected(iter.peek(), vec![Token::Return], "return statement")); }
        let expression = Expression::parse(iter)?;
        if iter.next_if_eq(&Token::Semicolon).is_none() { return Err(ParsingError::new_unexpected(iter.peek(), vec![Token::Semicolon], "return statement")); };

        Ok(Statement::new_return(expression))
    }

    fn parse_expression_statement<I: TokenIter> (iter: &mut Peekable<I>) -> Result<Statement, ParsingError> {
        let expression = Expression::parse(iter)?;
        iter.next_if_eq(&Token::Semicolon);

        Ok(Statement::new_expr(expression))
    }

    fn parse_block_statement<I: TokenIter> (iter : &mut Peekable<I>) -> Result<Statement, ParsingError> {
        Ok(Statement::new_block(BlockStatement::parse(iter)?))
    }
}

use std::fmt::{Display, Formatter};
impl Display for Statement {
    fn fmt (&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Let { identifier, expression } =>
                write!(f, "let {} = {};", identifier, expression),
            Statement::Return { expression } =>
                write!(f, "return {};", expression),
            Statement::Expression { expression } =>
                write!(f, "{}", expression),
            Statement::Block { body } =>
                write!(f, "{}", body),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BlockStatement {
    body: Vec<Statement>,
}


impl BlockStatement {
    pub fn parse<I: TokenIter>(iter: &mut Peekable<I>) -> Result<BlockStatement, ParsingError> {
        if (iter.next_if_eq(&Token::LeftCurly).is_none()) { return Err(ParsingError::new_unexpected(iter.peek(), vec![Token::LeftCurly], "block statement")); };
        let mut statements = vec![];
        let mut errors = vec![];
        loop {
            match iter.peek() {
                Some(&Token::RightCurly) => { iter.next(); break; },
                None => { return Err(ParsingError::new_other("unclosed curly brace")); },
                _ => {
                    match Statement::parse(iter) {
                        Ok(statement) => { statements.push(statement); },
                        Err(err) => { errors.push(err); },
                    }
                }
            }
        }

        if errors.is_empty() {
            return Ok(BlockStatement::new(statements));
        } else {
            return Err(ParsingError::new_multiple(errors));
        }
    }

    fn new(body: Vec<Statement>) -> BlockStatement {
        BlockStatement { body }
    }

    pub fn len(&self) -> usize {
        self.body.len()
    }
}
impl Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ block stmt len={} }}", self.len())
    }

}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::Token;

    #[test]
    fn test_statements() {
        struct Input {
            tokens: Vec<Token>,
            expected: Result<Statement, ParsingError>,
            next_tok: Option<Token>
        }
        let inputs = [
            Input {
                tokens: vec![],
                expected: Err(ParsingError::new_other("EOF")),
                next_tok: None,
            },

            Input {
                tokens: vec![
                    Token::Let,
                    Token::Identifier(String::from("x")),
                    Token::Assign,
                    Token::Integer(5),
                    Token::Semicolon,
                ],
                expected: Ok(Statement::new_let(IdentifierExpression::new("x"), Expression::new_int(5))),
                next_tok: None,
            },

            Input {
                tokens: vec![
                    Token::Return,
                    Token::Integer(5),
                    Token::Semicolon,
                ],
                expected: Ok(Statement::new_return(Expression::new_int(5))),
                next_tok: None,
            },
        ];

        for Input { tokens, expected, next_tok } in inputs {
            let mut iterator = tokens.into_iter().peekable();
            let parsed = Statement::parse(&mut iterator);

            assert_eq!(parsed, expected);
            assert_eq!(iterator.next(), next_tok);
        }
    }

    #[test]
    fn test_let_statements() {
        struct Input {
            tokens: Vec<Token>,
            expected: Result<Statement, ParsingError>,
            next_tok: Option<Token>,
        }
        let inputs = [
            Input {
                tokens: vec![
                    Token::Let,
                    Token::Identifier(String::from("x")),
                    Token::Assign,
                    Token::Integer(5),
                    Token::Semicolon,
                ],
                expected: Ok(Statement::new_let(IdentifierExpression::new("x"), Expression::new_int(5))),
                next_tok: None,
            },

            Input {
                tokens: vec![
                    Token::Let,
                    Token::Identifier(String::from("y")),
                    Token::Assign,
                    Token::Integer(10),
                    Token::Semicolon,
                ],
                expected: Ok(Statement::new_let(IdentifierExpression::new("y"), Expression::new_int(10))),
                next_tok: None,
            },

            Input {
                tokens: vec![
                    Token::Let,
                    Token::Identifier(String::from("foobar")),
                    Token::Assign,
                    Token::Integer(838383),
                    Token::Semicolon,
                ],
                expected: Ok(Statement::new_let(IdentifierExpression::new("foobar"), Expression::new_int(838383))),
                next_tok: None,
            },

            //missing let
            Input {
                tokens: vec![
                    Token::Return,
                    Token::Identifier(String::from("x")),
                ],
                expected: Err(ParsingError::new_unexpected(Some(&Token::Return), vec![Token::Let], "let statement")),
                next_tok: Some(Token::Return),
            },

            //missing assign
            Input {
                tokens: vec![
                    Token::Let,
                    Token::Identifier(String::from("x")),
                    Token::Bang,
                    Token::Integer(838383),
                    Token::Semicolon
                ],
                expected: Err(ParsingError::new_unexpected(Some(&Token::Bang), vec![Token::Assign], "let statement")),
                next_tok: Some(Token::Bang),
            },

            //missing semicolon
            Input {
                tokens: vec![
                    Token::Let,
                    Token::Identifier(String::from("x")),
                    Token::Assign,
                    Token::Integer(838383),
                ],
                expected: Err(ParsingError::new_unexpected(None, vec![Token::Semicolon], "let statement")),
                next_tok: None,
            },
        ];

        for Input {tokens, expected, next_tok} in inputs {
            let mut iterator = tokens.clone().into_iter().peekable();
            let parsed = Statement::parse_let_statement(&mut iterator);

            assert_eq!(parsed, expected, "{:?}", tokens);
            assert_eq!(iterator.next(), next_tok, "{:?}", tokens);
        }
    }

    #[test]
    fn test_return_statements() {
        struct Input {
            tokens: Vec<Token>,
            expected: Result<Statement, ParsingError>,
            next_tok: Option<Token>,
        }
        let inputs = [
            Input {
                tokens: vec![
                    Token::Return,
                    Token::Integer(5),
                    Token::Semicolon,
                ],
                expected: Ok(Statement::new_return(Expression::new_int(5))),
                next_tok: None,
            },

            Input {
                tokens: vec![
                    Token::Return,
                    Token::Integer(10),
                    Token::Semicolon,
                ],
                expected: Ok(Statement::new_return(Expression::new_int(10))),
                next_tok: None,
            },

            Input {
                tokens: vec![
                    Token::Return,
                    Token::Integer(993322),
                    Token::Semicolon,
                ],
                expected: Ok(Statement::new_return(Expression::new_int(993322))),
                next_tok: None,
            },

            //missing return
            Input {
                tokens: vec![
                    Token::Let,
                    Token::Integer(993322),
                ],
                expected: Err(ParsingError::new_unexpected(Some(&Token::Let), vec![Token::Return], "return statement")),
                next_tok: Some(Token::Let),
            },

            //missing semicolon
            Input {
                tokens: vec![
                    Token::Return,
                    Token::Integer(993322),
                ],
                expected: Err(ParsingError::new_unexpected(None, vec![Token::Semicolon], "return statement")),
                next_tok: None,
            },
        ];

        for Input {tokens, expected, next_tok} in inputs {
            let mut iterator = tokens.into_iter().peekable();
            let parsed = Statement::parse_return_statement(&mut iterator);

            assert_eq!(parsed, expected);
            assert_eq!(iterator.next(), next_tok);
        }
    }

    use crate::ast::expression::{IdentifierExpression, InfixOperator};

    #[test]
    fn test_block_statement() {
        struct Test {
            tokens: Vec<Token>,
            expected: Result<Statement, ParsingError>,
            next_tok: Option<Token>,
        }

        let tests = vec![
            Test {
                tokens: vec![
                    Token::LeftCurly,
                    Token::Let, Token::Identifier("x".to_string()), Token::Assign, Token::Integer(5), Token::Semicolon,
                    Token::Identifier("x".to_string()), Token::LessThan, Token::Integer(3), Token::Semicolon,
                    Token::RightCurly,
                ],
                expected: Ok(Statement::new_block(
                    BlockStatement::new(vec![
                        Statement::new_let(IdentifierExpression::new("x"), Expression::new_int(5)),
                        Statement::new_expr(Expression::new_infix(InfixOperator::LessThan, Expression::new_ident("x"), Expression::new_int(3))),
                    ]))),
                next_tok: None,
            },
            Test {
                tokens: vec![
                    Token::LeftCurly,
                    Token::Identifier("x".to_string()), Token::LessThan, Token::Integer(3), Token::Semicolon,
                    Token::RightCurly,
                ],
                expected: Ok(Statement::new_block(
                    BlockStatement::new(vec![
                        Statement::new_expr(Expression::new_infix(InfixOperator::LessThan, Expression::new_ident("x"), Expression::new_int(3))),
                    ]))),
                next_tok: None,
            },
            Test {
                tokens: vec![
                    Token::LeftCurly,
                    Token::RightCurly,
                ],
                expected: Ok(Statement::new_block(BlockStatement::new(vec![]))),
                next_tok: None,
            },
            Test {
                tokens: vec![
                    Token::LeftCurly,
                    Token::Let, Token::Identifier("x".to_string()), Token::Assign, Token::Integer(5), Token::Semicolon,
                    Token::Identifier("x".to_string()), Token::LessThan, Token::Integer(3), Token::Semicolon,
                ],
                expected: Err(ParsingError::new_other("unclosed curly brace")),
                next_tok: None,
            },
        ];

        for Test {tokens, expected, next_tok} in tests {
            let mut iterator = tokens.clone().into_iter().peekable();
            let parsed = Statement::parse_block_statement(&mut iterator);

            assert_eq!(parsed, expected, "{:?}", tokens);
            assert_eq!(iterator.next(), next_tok, "{:?}", tokens);
        }
    }
}
