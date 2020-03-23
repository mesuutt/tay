use std::io;
use crate::token::Token;
use crate::lexer::Lexer;
use crate::ast::{Program, LetStatement, Identifier};

pub fn start() {
    loop {
        let mut buf = String::new();
        match io::stdin().read_line(&mut buf) {
            Ok(_) => {
                let let_state = LetStatement{
                    token: Token::Ident(String::from("let")),
                    value: Identifier {
                        token: Token::Ident(String::from("myVar")),
                        value: String::from("myVar"),
                    },
                    name: Identifier {
                        token: Token::Ident(String::from("anotherVar")),
                        value: String::from("anotherVar"),
                    },
                };
                let b = Box::new(let_state);
                let mut statements= Vec::new();
                statements.push(b);
                let mut lexer = Lexer::new(buf.as_str());
                let mut p = Program {
                    statements,
                };
                println!("{}", lexer.next_token());
            }
            Err(_) => panic!("read from STDIN failed")
        }
    }
}