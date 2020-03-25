
#[derive(Debug, PartialEq)]
pub struct Ident(pub String);

#[derive(Debug, PartialEq)]
pub enum Expression {
    Ident(Ident),
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(Ident, Expression)
}

pub type BlockStatement = Vec<Statement>;
pub type Program = BlockStatement;


pub enum Precedence {
    Lowest
}