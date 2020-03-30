use std::fmt;
use std::fmt::{Formatter};

pub type IntegerSize = i64;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Token {
    Illegal(char),
    EndOfFile,
    Ident(String),
    Int(IntegerSize),

    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Percent,
    Caret,

    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,

    Function,
    Let,
    Boolean(bool),

    If,
    Else,
    Return,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Token::Illegal(c) => write!(f, "Illegal({})", c),
            Token::EndOfFile => write!(f, "EOF",),

            Token::Ident(s) => write!(f, "Identifier({})", s),
            Token::Int(i) => i.fmt(f),
            Token::Boolean(b) => write!(f, "Boolean({})", b),

            Token::Assign => write!(f, "="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Bang => write!(f, "!"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Caret => write!(f, "^"),
            Token::Percent => write!(f, "%"),

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
        "true" => Token::Boolean(true),
        "false" => Token::Boolean(false),
        "if" => Token::If,
        "else" => Token::Else,
        "return" => Token::Return,
        _ => Token::Ident(literal),
    }
}