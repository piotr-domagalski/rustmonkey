use crate::token::Token;

pub enum ParsingError {
    UnexpectedToken {
        got: Token,
        expected: Token,
        parsing_what: &'static str,
    },
    MultipleErrors {
        errors: Vec<ParsingError>,
    },
    OtherError {
        message: &'static str
    }
}

/*
 --------------------- expected something specific ---------------
--
ast/statement.rs-53-    fn parse_let_statement<I: TokenIter>(iter: &mut Peekable<I>) -> Result<Statement, &'static str> {
ast/statement.rs:54:        if iter.next_if_eq(&Token::Let).is_none() { return Err("let keyword expected"); };
ast/statement.rs-55-        let identifier = IdentifierExpression::parse(iter)?;
ast/statement.rs:56:        if iter.next_if_eq(&Token::Assign).is_none() { return Err("assignment operator expected"); };
ast/statement.rs-57-        let expression = Expression::parse(iter)?;
ast/statement.rs:58:        if iter.next_if_eq(&Token::Semicolon).is_none() { return Err("semicolon expected")};
ast/statement.rs-59-    
--
ast/statement.rs-64-    {
ast/statement.rs:65:        if iter.next_if_eq(&Token::Return).is_none() { return Err("return keyword expected"); };
ast/statement.rs-66-        let expression = Expression::parse(iter)?;
ast/statement.rs:67:        if iter.next_if_eq(&Token::Semicolon).is_none() { return Err("semicolon expected")};
ast/statement.rs-68-
--
ast/expression/operators.rs-17-            Token::Bang => Ok(Negation),
ast/expression/operators.rs:18:            _ => Err("Expected ! or - operator"),
ast/expression/operators.rs-19-        }
--
--
ast/expression.rs-128-            },
ast/expression.rs:129:            _ => Err("expected identifier token"),
ast/expression.rs-130-        }
--
ast/expression.rs-168-            _ => 
ast/expression.rs:169:                Err("expected integer literal token"),
ast/expression.rs-170-        }

 --------------------- other ---------------
ast/statement.rs-43-        match iter.peek() {
ast/statement.rs:44:            None => Err("EOF"),
ast/statement.rs-45-            Some(Token::Let) => Self::parse_let_statement(iter),
--
ast/statement.rs-47-            _ => Self::parse_expression_statement(iter),
ast/statement.rs:48:            // TODO: Should this be a wildcard? should there be an Err("unimplemented statement type")?
ast/statement.rs-49-        }


 -------- expected many options -------
ast/expression/operators.rs-70-            Token::NotEquals => Ok(NotEquals),
ast/expression/operators.rs:71:            _ => Err("invalid operator"),
ast/expression/operators.rs-72-        }
--
ast/expression.rs-53-            Some(Token::Bang) | Some(Token::Minus) => Expression::parse_prefix_expression(iter)?,
ast/expression.rs:54:            _ => return Err("unimplemented expression type")
ast/expression.rs-55-        };

------- multiple errors ------
ast/program.rs-36-        else {
ast/program.rs:37:            Err(errors)
ast/program.rs-38-        }

 --------------------- will go away after cleanup ---------------
--
ast/expression.rs-78-            Some(token) => PrefixOperator::parse(&token)?,
ast/expression.rs:79:            None => return Err("unexpected EOF"),
ast/expression.rs-80-        };

--
ast/expression.rs-91-        } else {
ast/expression.rs:92:            Err("expected operator token")
ast/expression.rs-93-        } 

 --------------------- not parsingerrors ---------------
--
ast/expression.rs-62-                        Ok(op) => op.precedence(),
ast/expression.rs:63:                        Err(_) => Precedence::Lowest,
ast/expression.rs-64-                    }
--
ast/program.rs-27-                Ok(statement) => statements.push(statement),
ast/program.rs:28:                Err("EOF") => break,
ast/program.rs:29:                Err(error) => errors.push(error),
ast/program.rs-30-            }
*/
