
#[derive(Debug, PartialEq, Clone)]
pub struct Ident(pub String);

#[derive(Debug, PartialEq)]
pub enum Expression {
    Ident(Ident),
    Literal(Literal),
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(Ident, Expression),
    Expression(Expression), // x + 10;
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Int(i64),
}

pub type BlockStatement = Vec<Statement>;
pub type Program = BlockStatement;


pub enum Precedence {
    Lowest
}