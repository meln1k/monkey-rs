use std::fmt::{Display, Formatter};
use std::fmt;
use crate::lexer::token::Token;


#[derive(Debug)]
pub enum Node {
    Prog(Program),
    Stat(Statement),
    Expr(Expression),
}

#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expr(ExpressionStatement),
}

#[derive(PartialEq, Debug)]
pub enum Expression {
    Identifier(String),
    IntegerLiteral(i64),
    PrefixExpression {
        operator: String,
        expr: Box<Expression>
    }
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>
}

#[derive(Debug)]
pub struct LetStatement {
    pub name: String,
    pub value: Expression,
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub return_value: Expression
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub expression: Expression
}


impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut out = String::new();

        for statement in &self.statements {
            out.push_str(&statement.to_string())
        }

        write!(f, "{}", out)
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let(LetStatement { name, value }) => {
                write!(f, "let {} = {};", name, value.to_string())
            }
            Statement::Return(ReturnStatement { return_value }) => {
                write!(f, "return {};", return_value)
            }
            Statement::Expr(ExpressionStatement { expression }) => {
                write!(f, "{}", expression)
            }
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Identifier(s) => {
                write!(f, "{}", s)
            },
            Expression::IntegerLiteral(n) => {
                write!(f, "{}", n)
            },
            Expression::PrefixExpression{operator, expr} => {
                write!(f, "({}{})", operator, expr)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::ast::{Program, LetStatement, Statement};
    use crate::ast::ast::Expression::Identifier;
    use crate::ast::ast::Node;

    #[test]
    fn test_to_string() {
        let program = Program {
            statements: vec![
                Statement::Let(LetStatement {
                    name: "myVar".to_owned(),
                    value: Identifier("anotherVar".to_owned()),
                })
            ]
        };

        let program_string = program.to_string();

        assert_eq!(program_string, "let myVar = anotherVar;", "unexpected to_string output")
    }
}