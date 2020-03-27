use std::fmt;
use std::borrow::Borrow;

#[derive(Debug, PartialEq, Clone)]
pub struct Ident(pub String);

#[derive(PartialEq, Debug)]
pub enum Prefix {
    Plus,
    Minus,
    Bang,
}


impl fmt::Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.borrow() {
            Prefix::Plus => write!(f, "+"),
            Prefix::Minus => write!(f, "-"),
            Prefix::Bang => write!(f, "!"),
        }
    }
}


#[derive(PartialEq, Debug)]
pub enum Infix {
    Plus,
    Minus,
    Mul,
    Div,
}

impl fmt::Display for Infix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.borrow() {
            Infix::Plus => write!(f, "+"),
            Infix::Minus => write!(f, "-"),
            Infix::Mul => write!(f, "*"),
            Infix::Div => write!(f, "/"),
        }
    }
}


#[derive(Debug, PartialEq)]
pub enum Expression {
    Ident(Ident),
    Literal(Literal),
    Prefix(Prefix, Box<Expression>),
    Infix(Infix, Box<Expression>, Box<Expression>),
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


#[derive(PartialOrd, PartialEq)]
pub enum Precedence {
    Lowest,
    Sum,  // +
    Product, // *
    Prefix, // -X, !X
}