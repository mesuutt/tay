use crate::token::Token;
use crate::ast::{Expression};

pub struct Identifier {
    token: Token,
    value: String,
}

impl Expression for Identifier {
    fn token_literal(&self) -> String {
        format!("Identifier: {}", 1)
    }
}
