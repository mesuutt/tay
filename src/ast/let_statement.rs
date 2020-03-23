use crate::token::Token;
use crate::ast::{Expression, Statement, Identifier};

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: dyn Expression,
}

impl Statement for LetStatement {
    fn token_literal(&self) -> String {
        String::from("statement")
    }
}

