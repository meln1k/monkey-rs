use crate::lexer::token::Token;



pub enum Node {
    Prog(Program),
    Stat(Statement),
    Expr(Expression)
}

pub enum Statement {
    Let(LetStatement)
}

pub enum Expression {
    Identifier(String)
}

pub struct Program {
    pub statements: Vec<Statement>
}

pub struct LetStatement {
    pub name: String,
    pub value: Expression
}

