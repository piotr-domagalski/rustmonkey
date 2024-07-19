use crate::token::Token;
use crate::ast::TokenIter;
use crate::ast::Expression;
use crate::ast::IdentifierExpression;
use std::iter::Peekable;

#[derive(Debug)]
pub enum Statement {
    Let{ identifier: IdentifierExpression, expression: Expression},
    Return { expression: Expression},
    Expression {expression: Expression},
    /*
    IfElse(IfElseStatement),
    Expression(ExpressionStatement),
    Empty,
    None,
    */
}

impl Statement {
    pub fn parse<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Statement, &'static str>
    {
        match iter.peek() {
            None => Err("EOF"),
            Some(Token::Let) => Self::parse_let_statement(iter),
            Some(Token::Return) => Self::parse_return_statement(iter),
            _ => Err("unimplemented statement type"),
        }
    }

    fn parse_let_statement<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Statement, &'static str> {
        if None == iter.next_if_eq(&Token::Let) {return Err("let keyword expected"); };

        let identifier = match iter.next() {
            Some(Token::Identifier(ident)) => crate::ast::IdentifierExpression{identifier: ident},
            _ => return Err("identifier expected"),
        };

        if None == iter.next_if_eq(&Token::Assign) { return Err("assignment operator expected"); };

        let expression = match Expression::parse(iter) {
            Ok(expr) => expr,
            _ => return Err("failed to parse expression"),
        };
    
        Ok(
            Statement::Let {
                    identifier: identifier,
                    expression: expression,
            }
        )
    }
    fn parse_return_statement<I: TokenIter> (iter: &mut Peekable<I>) -> Result<Statement, &'static str>
    {
        match iter.next() {
            Some(Token::Return) => (),
            _ => return Err("return keyword expected"),
        };
        let expression = match Expression::parse(iter) {
            Ok(expr) => expr,
            _ => return Err("failed to parse expression"),
        };
    
        Ok(
            Statement::Return {
                expression: expression,
            }
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::Token;

    #[test]
    fn test_let_statements() {
        struct Input {
            tokens: [Token; 5],
            identifier: &'static str,
            value: i64,
        }
        let inputs = [
            Input {
                tokens: [
                    Token::Let,
                    Token::Identifier(String::from("x")),
                    Token::Assign,
                    Token::Integer(5),
                    Token::Semicolon,
                ],
                identifier: "x",
                value: 5,
            },

            Input {
                tokens: [
                    Token::Let,
                    Token::Identifier(String::from("y")),
                    Token::Assign,
                    Token::Integer(10),
                    Token::Semicolon,
                ],
                identifier: "y",
                value: 10,
            },

            Input {
                tokens: [
                    Token::Let,
                    Token::Identifier(String::from("foobar")),
                    Token::Assign,
                    Token::Integer(838383),
                    Token::Semicolon,
                ],
                identifier: "foobar",
                value: 838383,
            },
        ];

        for input in inputs {
            let parsed = Statement::parse(&mut input.tokens.into_iter().peekable());
            if let Ok(Statement::Let{identifier: ident_expr, expression: _expr}) = parsed {
                assert_eq!(ident_expr.identifier, input.identifier);

                /*
                if let Expression::Literal(LiteralExpression::Integer(int)) = expr{
                    assert_eq!(int, input.value);
                } else {
                    panic!("Expected integer literal expression, got {:?}", expr);
                }
                */
            }
            else {
                panic!("Expected let statement, got {:?}", parsed)
            }
        }
    }

    #[test]
    fn test_return_statements() {
        struct Input {
            tokens: [Token; 3],
            value: i64,
        }
        let inputs = [
            Input {
                tokens: [
                    Token::Return,
                    Token::Integer(5),
                    Token::Semicolon,
                ],
                value: 5,
            },

            Input {
                tokens: [
                    Token::Return,
                    Token::Integer(10),
                    Token::Semicolon,
                ],
                value: 10,
            },

            Input {
                tokens: [
                    Token::Return,
                    Token::Integer(993322),
                    Token::Semicolon,
                ],
                value: 993322,
            }
        ];

        for input in inputs {
            let parsed = Statement::parse(&mut input.tokens.into_iter().peekable());
            if let Ok(Statement::Return{expression: expr}) = parsed {
                /*
                if let Expression::Literal(LiteralExpression::Integer(int)) = expr{
                    assert_eq!(int, input.value);
                } else {
                    panic!("Expected integer literal expression, got {:?}", expr);
                }
                */
            }
            else {
                panic!("Expected let statement, got {:?}", parsed)
            }
        }
    }
}
