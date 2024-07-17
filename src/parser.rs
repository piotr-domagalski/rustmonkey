use crate::lexer::Lexer;
use crate::token::Token;
use crate::ast::{Program, Statement, Expression};

use std::iter::Peekable;

pub struct Parser<I>
where I: Iterator<Item = Token>
{
    tokens: Peekable<I>
}

impl<I> Parser<I> 
where I: Iterator<Item = Token> 
{
    pub fn new(tokens: I) -> Parser<I> {
        Parser{
            tokens: tokens.peekable(),
        } 
    }

    pub fn parse_program(mut self) -> Result<Program, Vec<&'static str>> {
        let mut statements: Vec<Statement> = vec![];
        let mut errors: Vec<&str> = vec![];

        loop {
            match self.parse_statement() {
                Ok(statement) => statements.push(statement),
                Err("EOF") => break,
                Err(error) => errors.push(error),
            }
        }

        if errors.is_empty() {
            Ok(Program {statements})
        }
        else {
            Err(errors)
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, &'static str> {
        match self.tokens.peek() {
            None => Err("EOF"),
            Some(Token::Let) => self.parse_let_statement(),
            Some(Token::Return) => self.parse_return_statement(),
            _ => Err("unimplemented statement type"),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, &'static str> {
        match self.tokens.next() {
            Some(Token::Let) => (),
            _ => return Err("let keyword expected"),
        }
        let identifier = match self.tokens.next() {
            Some(Token::Identifier(ident)) => crate::ast::IdentifierExpression{value: ident},
            _ => return Err("identifier expected"),
        };
        match self.tokens.next() {
            Some(Token::Assign) => (),
            _ => return Err("assignment operator expected"),
        };
        let expression = match self.parse_expression() {
            Ok(expr) => expr,
            _ => return Err("failed to parse expression"),
        };

        Ok(
            Statement::Let(
                crate::ast::LetStatement {
                    identifier: identifier,
                    value: expression,
                }
            )
        )
    }
    fn parse_return_statement(&mut self) -> Result<Statement, &'static str> {
        match self.tokens.next() {
            Some(Token::Return) => (),
            _ => return Err("return keyword expected"),
        };
        let expression = match self.parse_expression() {
            Ok(expr) => expr,
            _ => return Err("failed to parse expression"),
        };

        Ok(
            Statement::Return(
                crate::ast::ReturnStatement {
                    value: expression,
                }
            )
        )
    }

    fn parse_expression(&mut self) -> Result<Expression, &'static str> {
        while self.tokens.next_if(|tok| *tok != Token::Semicolon).is_some() {}
        self.tokens.next();
        Ok(Expression::Identifier(crate::ast::IdentifierExpression{value: String::from("haha fooled ya")}))
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser_new() {
        let tokens = vec![Token::Let, Token::Identifier(String::from("x")), Token::Assign, Token::Integer(5), Token::Semicolon];
        let _parser_from_vec = Parser::new(tokens.into_iter());

        let code = "let x = 5;";
        let lexer = Lexer::new(code);
        let _parser_from_lexer = Parser::new(lexer);
    }

    #[test]
    fn test_let_statements() {
        let input = [
            Token::Let,
            Token::Identifier(String::from("x")),
            Token::Assign,
            Token::Integer(5),
            Token::Semicolon,

            Token::Let,
            Token::Identifier(String::from("y")),
            Token::Assign,
            Token::Integer(10),
            Token::Semicolon,

            Token::Let,
            Token::Identifier(String::from("foobar")),
            Token::Assign,
            Token::Integer(838383),
            Token::Semicolon,
        ];
        let expected = vec![("x", 5), ("y", 10), ("foobar", 838383)];
        let parser = Parser::new(input.into_iter());

        let output: Program = parser.parse_program().expect("hardcoded tokens shouldn't fail to parse");

        assert_eq!(output.statements.len(), 3, "expected 3 statements, got {}", output.statements.len());

        for (i, statement) in output.statements.iter().enumerate() {
            if let Statement::Let(let_statement) = statement {
                assert_eq!(let_statement.identifier.value, expected[i].0);
                /*
                if let Expression::Literal(literal_expr) = &let_statement.value {
                    if let crate::ast::LiteralExpression::Integer(val) = literal_expr{
                        assert_eq!(*val, expected[i].1);
                    } else {
                        panic!("expected LiteralExpression::Integer, got {:?}", literal_expr);
                    }
                } else {
                    panic!("expected Expression::Literal, got {:?}", let_statement.value);
                }
                */
            } else {
                panic!("expected Statement::Let, got {:?}", statement);
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let input = [
            Token::Return,
            Token::Integer(5),
            Token::Semicolon,

            Token::Return,
            Token::Integer(10),
            Token::Semicolon,

            Token::Return,
            Token::Integer(993322),
            Token::Semicolon,
        ];

        let parser = Parser::new(input.into_iter());

        let output: Program = parser.parse_program().expect("hardcoded tokens shouldn't fail to parse");

        assert_eq!(output.statements.len(), 3, "expected 3 statements, got {}", output.statements.len());

        for (i, statement) in output.statements.iter().enumerate() {
            if let Statement::Return(return_statement) = statement {
                /*
                if let Expression::Literal(literal_expr) = &let_statement.value {
                    if let crate::ast::LiteralExpression::Integer(val) = literal_expr{
                        assert_eq!(*val, expected[i].1);
                    } else {
                        panic!("expected LiteralExpression::Integer, got {:?}", literal_expr);
                    }
                } else {
                    panic!("expected Expression::Literal, got {:?}", let_statement.value);
                }
                */
            } else {
                panic!("expected Statement::Return, got {:?}", statement);
            }
        }
    }
}
