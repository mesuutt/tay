use std::io;
use crate::token::Token;
use crate::lexer::Lexer;
use crate::ast::{Program, Statement, Expression, Ident};

pub fn start() {
    loop {
        let mut buf = String::new();
        match io::stdin().read_line(&mut buf) {
            Ok(_) => {
                let let_state = Statement::Let(
                    Ident(String::from("myVar")),
                    Expression::Ident(Ident(String::from("anotherVar"))),
                );
                let mut lexer = Lexer::new(buf.as_str());
                let mut p = Program::new();
                p.push(let_state);
                while let t = lexer.next_token() {
                    println!("{}", t);
                    if t == Token::EndOfFile {
                        break;
                    }
                }
            }
            Err(_) => panic!("read from STDIN failed")
        }
    }
}