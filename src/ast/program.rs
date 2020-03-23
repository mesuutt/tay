pub trait Node {}

// pub trait Statement {}
pub trait Statement: Node {
    fn token_literal(&self) -> String;
}

/*impl Node for Statement {
    fn token_literal() -> String {
        String::from("statement")
    }
}*/

// pub trait Expression {}
pub trait Expression: Node {
    fn token_literal(&self) -> String;
}

pub struct Program<T: Statement> {
    statements: Vec<Box<T>>,
}

impl<T> Program<T> where T: Statement {
    pub fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            String::from("")
        }
    }
}