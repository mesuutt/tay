use std::fmt;
use crate::parser::ParseError;

pub type IntegerSize = i64;
pub type FloatSize = f64;

#[derive(Debug, PartialEq, Clone)]
pub struct Ident(pub String);

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(PartialEq, Debug, Clone)]
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


#[derive(PartialEq, Debug, Clone)]
pub enum Infix {
    Plus,
    Minus,
    Mul,
    Div,
    Exponent,
    Percent,

    Gt,
    Lt,
    Gte,
    Lte,
    Eq,
    NotEq,
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

            Infix::Gt => write!(f, ">"),
            Infix::Lt => write!(f, "<"),
            Infix::Lte => write!(f, "<="),
            Infix::Gte => write!(f, ">="),
            Infix::Eq => write!(f, "=="),
            Infix::NotEq => write!(f, "!="),
        }
    }
}


#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Ident(Ident),
    Literal(Literal),
    Prefix(Prefix, Box<Expression>),
    Infix(Infix, Box<Expression>, Box<Expression>),

    // myFunc(x, y)
    Call {
        // func should be Ident or Expression::Func
        func: Box<Expression>,
        args: Vec<Expression>,
    },

    // fn (x, y) {}
    Func {
        identifier: Option<Ident>, // name of the function
        params: Vec<Ident>,
        body: BlockStatement,
    },

    If {
        condition: Box<Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
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
            Expression::Call { func, args } => {
                let arg_list = args.iter().map(|expr| format!("{}", expr)).collect::<Vec<String>>();
                write!(f, "{}({})", func, arg_list.join(", "))
            }
            Expression::Func { identifier: name, params, body } => {
                let param_list = params.iter().map(|s| format!("{}", s)).collect::<Vec<String>>();
                let statement_list = body.into_iter().map(|s| format!("{}", s)).collect::<Vec<String>>();
                write!(f, "fn {}({}) {{\n{}\n}}", name.clone().unwrap_or(Ident("".to_owned())), param_list.join(", "), statement_list.join(""))
            }

            Expression::If { condition, consequence, alternative } => {
                if !alternative.is_some() {
                    write!(f, "if {} {} else {}",
                           condition,
                           consequence.iter().map(|x| format!("{}", x)).collect::<Vec<String>>().join(""),
                           alternative.as_ref().unwrap().iter().map(|x| format!("{}", x)).collect::<Vec<String>>().join(""),
                    )
                } else {
                    write!(f, "if {} {}",
                           condition,
                           consequence.iter().map(|x| format!("{}", x)).collect::<Vec<String>>().join("")
                    )
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(Ident, Expression),
    Return(Expression),

    // x + 10;
    Expression(Expression),
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

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Int(IntegerSize),
    Float(FloatSize),
    String(String),
    Bool(bool),
}


impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Int(x) => write!(f, "{}", x),
            Literal::Float(x) => write!(f, "{}", x),
            Literal::String(x) => write!(f, "{}", x),
            Literal::Bool(x) => write!(f, "{}", x),
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
    // == or !=
    Equal,
    // < or >
    LessGreater,
    // + or -
    Sum,
    // *, /, %
    Product,
    // ^
    Exponent,
    // -X, !X
    Prefix,
    // myFunc(x), LParen
    Call,
}