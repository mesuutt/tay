use std::fmt;

pub type IntegerSize = i64;

#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    Illegal(char),
    EndOfFile,
    Ident(String),
    Int(IntegerSize),
    Assign,
    Plus,

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