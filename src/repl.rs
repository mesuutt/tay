use std::io;
use crate::token::Token;
use crate::lexer::Lexer;
use crate::ast::{Program, Statement, Expression, Identifier};

pub fn start() {
    loop {
        let mut buf = String::new();
        match io::stdin().read_line(&mut buf) {
            Ok(_) => {
                let let_state = Statement::Let(
                    Identifier {
                        token: Token::Ident(String::from("myVar")),
                        value: String::from("myVar"),
                    },
                    Expression::Identifier(Identifier {
                        token: Token::Ident(String::from("anotherVar")),
                        value: String::from("anotherVar"),
                    }),
                );
                let mut lexer = Lexer::new(buf.as_str());
                let mut p = Program::new();
                p.push(let_state);
                println!("{}", lexer.next_token(),);
            }
            Err(_) => panic!("read from STDIN failed")
        }
    }
}