use std::io;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::evaluator::{Evaluator, Env};
use std::io::Write;
use rustyline::Editor;
use rustyline::error::ReadlineError;


pub fn start() {
    let env = Env::new();
    let mut evaluator = Evaluator::new(env);
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let lexer = Lexer::new(line.as_str());
                let mut p = Parser::new(lexer);
                let prog = p.parse();
                if !p.get_errors().is_empty() {
                    let errors = p.get_errors();
                    errors.iter().map(|e| {
                        println!("Parse error: {}", e);
                    });
                    continue;
                }
                match evaluator.eval(prog) {
                    Some(obj) => {
                        println!("{}", obj)
                    },
                    None => continue
                }
            },
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break
            },
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }
    rl.save_history("history.txt").unwrap();
}