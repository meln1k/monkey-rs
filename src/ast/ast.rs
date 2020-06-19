
#[derive(Debug)]
pub enum Node {
    Prog(Program),
    Stat(Statement),
    Expr(Expression)
}

#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement)
}

#[derive(Debug)]
pub enum Expression {
    Identifier(String)
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>
}

#[derive(Debug)]
pub struct LetStatement {
    pub name: String,
    pub value: Expression
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub return_value: Expression
}

