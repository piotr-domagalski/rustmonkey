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
            Some(_) => self.parse_expression_statement(),
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

        /*
        let expression = match self.parse_expression() {
            Ok(expr) => expr,
            _ => return Err("failed to parse expression"),
        };
        */
        while self.tokens.next_if(|tok| *tok != Token::Semicolon).is_some() {}
        let expression = Expression::Identifier(crate::ast::IdentifierExpression{value: String::from("haha fooled ya")});

        match self.tokens.next() {
            Some(Token::Semicolon) => (),
            _ => return Err("semicolon expected"),
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
        /*
        let expression = match self.parse_expression() {
            Ok(expr) => expr,
            _ => return Err("failed to parse expression"),
        };
        */
        while self.tokens.next_if(|tok| *tok != Token::Semicolon).is_some() {}
        let expression = Expression::Identifier(crate::ast::IdentifierExpression{value: String::from("haha fooled ya")});

        match self.tokens.next() {
            Some(Token::Semicolon) => (),
            _ => return Err("semicolon expected"),
        };

        Ok(
            Statement::Return(
                crate::ast::ReturnStatement {
                    value: expression,
                }
            )
        )
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, &'static str> {
        let expression = match self.parse_expression() {
            Ok(expr) => expr,
            _ => return Err("failed to parse expression")
        };
        self.tokens.next_if(|tok| *tok == Token::Semicolon);

        Ok( Statement::Expression(
                crate::ast::ExpressionStatement {
                    value: expression
                }
            )
        )
    }

    fn parse_expression(&mut self) -> Result<Expression, &'static str> {
        //prefix
        let left_expression = match self.tokens.peek() {
            Some(Token::Identifier(_)) => self.parse_identifier_expression(),
            Some(Token::Integer(_)) => self.parse_integer_expression(),
            _ => Err("unimplemented expression type")
        };

        //TEMP
        self.tokens.next();

        left_expression
    }

    fn parse_identifier_expression(&mut self) -> Result<Expression, &'static str> {
        if let Some(Token::Identifier(ident)) = self.tokens.peek() {
            Ok(Expression::Identifier(crate::ast::IdentifierExpression { value:  ident.clone()}))
        } else {
            Err("expected identifier token")
        }
    }

    fn parse_integer_expression(&mut self) -> Result<Expression, &'static str> {
        if let Some(Token::Integer(int)) = self.tokens.peek() {
            Ok(Expression::Literal(crate::ast::LiteralExpression::Integer(*int)))
        } else {
            Err("epected integer literal token")
        }

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

    #[test]
    fn test_identifier_expression() {
        let input = [
            Token::Identifier(String::from("foobar")),
            Token::Semicolon,
        ];

        let parser = Parser::new(input.into_iter());

        let output: Program = parser.parse_program().expect("hardcoded tokens shouldn't fail to parse");

        assert_eq!(output.statements.len(), 1, "expected 1 statements, got {}", output.statements.len());

        if let Statement::Expression(expression_statement) = &output.statements[0] {
            if let Expression::Identifier(ident) = &expression_statement.value {
                assert_eq!(ident.value, "foobar");
            } else {
                panic!("expected Expression::Identifier, got {:?}", expression_statement.value)

            }

        } else {
            panic!("expected Statement::Expression, got {:?}", output.statements[0]);
        }
    }

    #[test]
    fn test_integer_expression() {
        let input = [
            Token::Integer(5),
            Token::Semicolon,
        ];

        let parser = Parser::new(input.into_iter());

        let output: Program = parser.parse_program().expect("hardcoded tokens shouldn't fail to parse");

        assert_eq!(output.statements.len(), 1, "expected 1 statements, got {}", output.statements.len());

        if let Statement::Expression(expression_statement) = &output.statements[0] {
            if let Expression::Literal(crate::ast::LiteralExpression::Integer(int)) = &expression_statement.value {
                assert_eq!(*int, 5);
            } else {
                panic!("expected Expression::Literal(Integer), got {:?}", expression_statement.value)

            }

        } else {
            panic!("expected Statement::Expression, got {:?}", output.statements[0]);
        }
    }

    #[test]
    fn test_prefix_inverse() {
        let input = [Token::Minus, Token::Integer(5), Token::Semicolon];

        let parser = Parser::new(input.into_iter());

        let output: Program = parser.parse_program().expect("hardcoded tokens shouldn't fail to parse");

        assert_eq!(output.statements.len(), 1, "expected 1 statements, got {}", output.statements.len());
    
        if let Statement::Expression(expression_statement) = &output.statements[0] {
            if let Expression::Prefix(boxed_expression) = expression_statement.value {
                if let crate::ast::PrefixExpression::Inverse(expression) = *boxed_expression {
                    if let Expression::Literal(crate::ast::LiteralExpression::Integer(int)) = &expression{
                        assert_eq!(*int, 5);
                    } else {
                        panic!("expected Expression::Literal(Integer), got {:?}", expression);
                    }
                } else {
                    panic!("idk anymore");
                }
            }
            else {
                panic!("expected Expression::Prefix::Inverse, got {:?}", expression_statement);
            }
        } else {
            panic!("expected Statement::Expression, got {:?}", output.statements[0]);
        }
    }
}
