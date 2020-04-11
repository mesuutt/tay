use std::fmt;
use std::fmt::{Formatter};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Illegal(char),
    EndOfFile,
    Ident(String),
    Int(String),
    Float(String),
    Bool(bool),
    
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Percent,
    Caret,

    Gt,
    Lt,
    Gte,
    Lte,
    Eq,
    NotEq,

    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Let,

    If,
    Else,
    Return,
    Function,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Token::Illegal(c) => write!(f, "{}", c),
            Token::EndOfFile => write!(f, "EOF",),

            Token::Ident(s) => write!(f, "{}", s),
            Token::Int(i) => i.fmt(f),
            Token::Float(i) => i.fmt(f),
            Token::Bool(b) => write!(f, "{}", b),

            Token::Assign => write!(f, "="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Bang => write!(f, "!"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Caret => write!(f, "^"),
            Token::Percent => write!(f, "%"),

            Token::Lt => write!(f, "<"),
            Token::Gt => write!(f, ">"),
            Token::Lte => write!(f, "<="),
            Token::Gte => write!(f, "=<"),
            Token::Eq => write!(f, "=="),
            Token::NotEq => write!(f, "!="),

            Token::Comma => write!(f, ","),
            Token::Semicolon => write!(f, ";"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),

            Token::Function => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::Return => write!(f, "return"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
        }
    }
}

pub fn lookup_ident(literal: String) -> Token {
    match literal.as_str() {
        "fn" => Token::Function,
        "let" => Token::Let,
        "true" => Token::Bool(true),
        "false" => Token::Bool(false),
        "if" => Token::If,
        "else" => Token::Else,
        "return" => Token::Return,
        _ => Token::Ident(literal),
    }
}