use std::fmt;
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum Node {
    Prog(Program),
    Stat(Statement),
    Expr(Expression),
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expr(ExpressionStatement),
    Block(BlockStatement),
}

#[derive(PartialEq, Debug)]
pub enum Expression {
    Identifier(String),
    IntegerLiteral(i64),
    PrefixExpression {
        operator: Operator,
        expr: Box<Expression>,
    },
    InfixExpression {
        left: Box<Expression>,
        operator: Operator,
        right: Box<Expression>,
    },
    Boolean(bool),
    IfExpression {
        condition: Box<Expression>,
        consequence: Box<BlockStatement>,
        alternative: Option<Box<BlockStatement>>,
    },
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(PartialEq, Debug)]
pub struct LetStatement {
    pub name: String,
    pub value: Expression,
}

#[derive(PartialEq, Debug)]
pub struct ReturnStatement {
    pub return_value: Expression,
}

#[derive(PartialEq, Debug)]
pub struct ExpressionStatement {
    pub expression: Expression,
}

#[derive(PartialEq, Debug)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

#[allow(non_camel_case_types)]
#[derive(PartialEq, Debug)]
pub enum Operator {
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,
    EQ,
    NOT_EQ,
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
            Statement::Expr(ExpressionStatement { expression }) => write!(f, "{}", expression),
            Statement::Block(block_statement) => write!(f, "{}", block_statement.to_string()),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Identifier(s) => write!(f, "{}", s),
            Expression::IntegerLiteral(n) => write!(f, "{}", n),
            Expression::PrefixExpression { operator, expr } => write!(f, "({}{})", operator, expr),
            Expression::InfixExpression {
                left,
                operator,
                right,
            } => write!(f, "({} {} {})", left, operator, right),
            Expression::Boolean(b) => write!(f, "{}", b),
            Expression::IfExpression {
                condition,
                consequence,
                alternative,
            } => {
                write!(f, "if{} {}", condition.to_string(), consequence.to_string())?;
                match alternative {
                    Some(block) => write!(f, "else {}", block.to_string()),
                    None => Ok(()),
                }
            }
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let str = match self {
            Operator::ASSIGN => "=",
            Operator::PLUS => "+",
            Operator::MINUS => "-",
            Operator::BANG => "!",
            Operator::ASTERISK => "*",
            Operator::SLASH => "/",
            Operator::LT => "<",
            Operator::GT => ">",
            Operator::EQ => "==",
            Operator::NOT_EQ => "!=",
        };
        write!(f, "{}", str)
    }
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for s in &self.statements {
            write!(f, "{}", s.to_string())?
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::ast::Expression::Identifier;
    use crate::ast::ast::{LetStatement, Program, Statement};

    #[test]
    fn test_to_string() {
        let program = Program {
            statements: vec![Statement::Let(LetStatement {
                name: "myVar".to_owned(),
                value: Identifier("anotherVar".to_owned()),
            })],
        };

        let program_string = program.to_string();

        assert_eq!(
            program_string, "let myVar = anotherVar;",
            "unexpected to_string output"
        )
    }
}
