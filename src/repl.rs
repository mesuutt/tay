use std::io;
use crate::token::Token;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::ast::{Program, Statement, Expression, Ident};

pub fn start() {
    loop {
        let mut buf = String::new();
        match io::stdin().read_line(&mut buf) {
            Ok(_) => {
                let mut lexer = Lexer::new(buf.as_str());
                let mut p = Parser::new(lexer);
                let prog = p.parse();
                for x in prog {
                    println!("{}", x);
                }
            }
            Err(_) => panic!("read from STDIN failed")
        }
    }
}