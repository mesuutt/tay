use crate::ast::Identifier;


pub enum Expression {
    Identifier(Identifier),
}

pub enum Statement {
    Let(Identifier, Expression)
}

pub type BlockStatement = Vec<Statement>;
pub type Program = BlockStatement;