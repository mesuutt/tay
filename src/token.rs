use std::fmt;

pub type IntegerSize = i64;

#[derive(Debug)]
pub enum Token {
    Illegal(char),
    EndOfFile,
    Identifier(String),
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
}