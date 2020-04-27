mod lexer;
mod ast;
mod parser;
mod evaluator;
mod repl;

use std::{env, process};
use std::fs::File;
use std::io::Read;

use crate::evaluator::{Env, eval};
use std::rc::Rc;
use std::cell::RefCell;
use crate::lexer::Lexer;
use crate::parser::Parser;

fn main() {
    if let Some(filename) = env::args().nth(1) {
        eval_file(&filename);
    } else {
        repl::start();
    }
}

fn eval_file(filename: &str)  {
    let mut file = match File::open(filename) {
        Ok(f) => f,
        Err(_) => {
            println!("file not found: '{}'", filename);
            process::exit(1);
        }
    };

    let mut source = String::new();
    if let Err(_e) = file.read_to_string(&mut source) {
        println!("error reading file");
        process::exit(1);
    }

    let env = Env::new();
    let program = Parser::new(Lexer::new(source)).parse();
    if let Some(err) = program.error {
        println!("Parse error: {}", err);
        process::exit(1);
    }

    match eval(program, Rc::new(RefCell::new(env))) {
        Ok(obj) => println!("{}", obj),
        Err(e) => println!("ERROR: {}", e)
    }
}