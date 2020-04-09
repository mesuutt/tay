use std::fmt;
use crate::parser::ParseError;

pub type IntegerSize = i64;
pub type FloatSize = f64;

#[derive(Debug, PartialEq, Clone)]
pub struct Ident(pub String);

#[derive(PartialEq, Debug)]
pub enum Prefix {
    Minus,
    Bang,
}

impl fmt::Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
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
    Exponent,
    Percent,
}

impl fmt::Display for Infix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Infix::Plus => write!(f, "+"),
            Infix::Minus => write!(f, "-"),
            Infix::Mul => write!(f, "*"),
            Infix::Div => write!(f, "/"),
            Infix::Percent => write!(f, "%"),
            Infix::Exponent => write!(f, "^"),
        }
    }
}


#[derive(Debug, PartialEq)]
pub enum Expression {
    Ident(Ident),
    Literal(Literal),
    Prefix(Prefix, Box<Expression>),
    Infix(Infix, Box<Expression>, Box<Expression>),
    Call {
        func: Box<Expression>,
        args: Vec<Expression>,
    },
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
            Expression::Call { func: _, args } => {
                let arg_list = args.iter().map(|expr| format!("{}", expr)).collect::<Vec<String>>();
                write!(f, "({})", arg_list.join(", "))
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(Ident, Expression),
    Expression(Expression), // x + 10;
    Return(Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let(ident, expr) => {
                write!(f, "let {} = {};", ident.0, expr)
            }
            Statement::Expression(expr) => write!(f, "{}", expr),
            Statement::Return(expr) => write!(f, "{}", expr),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Int(IntegerSize),
    Float(FloatSize),
}


impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Int(x) => write!(f, "{}", x),
            Literal::Float(x) => write!(f, "{}", x),
        }
    }
}


pub type BlockStatement = Vec<Statement>;

pub struct Program {
    pub statements: BlockStatement,
    pub errors: Vec<ParseError>,
}


#[derive(PartialOrd, PartialEq)]
pub enum Precedence {
    Lowest,
    Sum, // +
    Product, // *, /, %
    Exponent, // ^
    Prefix, // -X, !X
    Call, // myFunc(x), LParen
}