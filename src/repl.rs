use std::io;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::evaluator::{Evaluator, Env};
use std::io::Write;


pub fn start() {

    loop {
        print!(">> ");
        io::stdout().flush();
        let mut buf = String::new();
        match io::stdin().read_line(&mut buf) {
            Ok(_) => {
                let lexer = Lexer::new(buf.as_str());
                let mut p = Parser::new(lexer);
                let prog = p.parse();
                if !p.get_errors().is_empty() {
                    let errors = p.get_errors();
                    errors.iter().map(|e| {
                        println!("Parse error: {}", e);
                    });
                    continue
                }

                for x in prog.iter() {
                    println!("{}", x);
                }
            }
            Err(_) => panic!("read from STDIN failed")
        }
    }
}