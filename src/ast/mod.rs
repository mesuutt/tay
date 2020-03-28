use std::fmt;

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
        match self {
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
        match self {
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

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Ident(ident) => {
                write!(f, "{}", ident.0)
            }
            Expression::Literal(literal) => {
                write!(f, "{}", literal)
            }
            Expression::Prefix(prefix, expr) => {
                write!(f, "({}{})", prefix, expr)
            }
            Expression::Infix(infix, left, right) => {
                write!(f, "({} {} {})", left, infix, right)
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(Ident, Expression),
    Expression(Expression), // x + 10;
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let(ident, expr) => {
                write!(f, "let {} = {};", ident.0, expr)
            }
            Statement::Expression(expr) => write!(f, "{}", expr)
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Int(i64),
}


impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Int(int) => write!(f, "{}", int),
        }
    }
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