use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::evaluator::{Evaluator, Env};
use rustyline::Editor;
use rustyline::error::ReadlineError;
use std::cell::RefCell;
use std::rc::Rc;


pub fn start() {
    let env = Env::new();
    let mut evaluator = Evaluator::new(Rc::new(RefCell::new(env)));
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
                let program = p.parse();
                if !program.errors.is_empty() {
                    for err in program.errors.iter() {
                        println!("Parse error: {}", err);
                    };
                    continue;
                }
                match evaluator.eval(program) {
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