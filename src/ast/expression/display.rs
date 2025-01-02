use std::fmt::{Display, Formatter};
use crate::ast::{Expression, IdentifierExpression, Literal};

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            Expression::Identifier{identifier_expression} =>
                write!(f, "{}", identifier_expression),
            Expression::Literal{literal} => 
                write!(f, "{}", literal),
            Expression::Prefix{operator, expression} => 
                write!(f, "({}{})", operator, expression),
            Expression::Infix{operator, left, right} =>
                write!(f, "({} {} {})", left, operator, right),
            Expression::If{condition, consequence, alternative: None} =>
                write!(f, "if {} {}", condition, consequence),
            Expression::If{condition, consequence, alternative: Some(alt)} =>
                write!(f, "if {} {} else {}", condition, consequence, alt),
            Expression::Call{callable, arguments} => {
                let mut arg_string = "".to_string();
                if arguments.len() != 0 {
                    arg_string += &format!("{}", arguments[0]);
                    for arg in &arguments[1..] {
                        arg_string += &format!(", {}", arg);
                    }
                }
                write!(f, "{}({})", callable, arg_string)
            }
        }
    }
}

impl Display for IdentifierExpression {
    fn fmt (&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.identifier)
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            Literal::Integer(int) => write!(f, "{}", int),
            Literal::Bool(boolean) => write!(f, "{}", boolean),
            Literal::Function {parameters, body} => {
                // TODO: make this print everything, not just counts
                write!(f, "fn({} parameters){}", parameters.len(), body)
            },
        }
    }
}
