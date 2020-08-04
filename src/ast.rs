use std::collections::HashMap;
use std::fmt;
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum Node {
    Prog(Program),
    Stat(Statement),
    Expr(Expression),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expr(ExpressionStatement),
    Block(BlockStatement),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Ident(Identifier),
    IntegerLiteral(i64),
    FloatLiteral(f64),
    PrefixExpression {
        operator: PrefixOperator,
        expr: Box<Expression>,
    },
    InfixExpression {
        left: Box<Expression>,
        operator: InfixOperator,
        right: Box<Expression>,
    },
    Boolean(bool),
    IfExpression {
        condition: Box<Expression>,
        consequence: Box<BlockStatement>,
        alternative: Option<Box<BlockStatement>>,
    },
    FunctionLiteral {
        parameters: Vec<Identifier>,
        body: BlockStatement,
    },
    CallExpression {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
    StringLiteral {
        value: String,
    },
    ArrayLiteral {
        elements: Vec<Expression>,
    },
    IndexExpression {
        left: Box<Expression>,
        index: Box<Expression>,
    },
    HashLiteral {
        pairs: Vec<(Box<Expression>, Box<Expression>)>,
    },
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct LetStatement {
    pub name: Identifier,
    pub value: Expression,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ReturnStatement {
    pub return_value: Expression,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ExpressionStatement {
    pub expression: Expression,
}

#[derive(PartialEq, Debug, Clone)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
pub struct Identifier(pub String);

#[allow(non_camel_case_types)]
#[derive(PartialEq, Debug, Clone)]
pub enum InfixOperator {
    PLUS,
    MINUS,
    ASTERISK,
    SLASH,
    LT,
    GT,
    EQ,
    NOT_EQ,
}

#[derive(PartialEq, Debug, Clone)]
pub enum PrefixOperator {
    MINUS,
    BANG,
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
                write!(f, "let {} = {};", name.0, value.to_string())
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
            Expression::Ident(Identifier(s)) => write!(f, "{}", s),
            Expression::IntegerLiteral(n) => write!(f, "{}", n),
            Expression::FloatLiteral(n) => write!(f, "{}", n),
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
            Expression::FunctionLiteral { parameters, body } => write!(
                f,
                "fn({}){}",
                parameters
                    .iter()
                    .map(|i| i.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                body.to_string()
            ),
            Expression::CallExpression {
                function,
                arguments,
            } => write!(
                f,
                "{}({})",
                function,
                arguments
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Expression::StringLiteral { value } => write!(f, "{}", value),
            Expression::ArrayLiteral { elements } => {
                let elems: Vec<String> = elements.iter().map(|e| e.to_string()).collect();
                write!(f, "[{}]", elems.join(", "))
            }
            Expression::IndexExpression { left, index } => {
                write!(f, "({}[{}])", left.to_string(), index.to_string())
            }
            Expression::HashLiteral { pairs } => {
                let strings: Vec<String> = pairs
                    .iter()
                    .map(|(k, v)| format!("{}:{}", *k, *v))
                    .collect();
                write!(f, "{{{}}}", strings.join(", "))
            }
        }
    }
}

impl Display for InfixOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let str = match self {
            InfixOperator::PLUS => "+",
            InfixOperator::MINUS => "-",
            InfixOperator::ASTERISK => "*",
            InfixOperator::SLASH => "/",
            InfixOperator::LT => "<",
            InfixOperator::GT => ">",
            InfixOperator::EQ => "==",
            InfixOperator::NOT_EQ => "!=",
        };
        write!(f, "{}", str)
    }
}

impl Display for PrefixOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let str = match self {
            PrefixOperator::MINUS => "-",
            PrefixOperator::BANG => "!",
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

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Expression::Ident;
    use crate::ast::{Identifier, LetStatement, Program, Statement};

    #[test]
    fn test_to_string() {
        let program = Program {
            statements: vec![Statement::Let(LetStatement {
                name: Identifier("myVar".to_owned()),
                value: Ident(Identifier("anotherVar".to_owned())),
            })],
        };

        let program_string = program.to_string();

        assert_eq!(
            program_string, "let myVar = anotherVar;",
            "unexpected to_string output"
        )
    }
}
