use std::fs::File;
use std::io::Read;

use tay::evaluator::{Env, eval, Object};
use std::rc::Rc;
use std::cell::RefCell;
use tay::lexer::Lexer;
use tay::parser::Parser;

pub(crate) fn eval_file(filename: &str) -> Result<Object, String> {
    let mut file = File::open(filename).map_err(|_e| format!("file not found: '{}'", filename))?;
    let mut source = String::new();

    file.read_to_string(&mut source).map_err(|_e| "error reading file")?;

    let env = Env::new();
    let program = Parser::new(Lexer::new(source)).parse().map_err(|e| format!("Parse error: {}", e))?;
    let obj = eval(program, Rc::new(RefCell::new(env))).map_err(|e| format!("Eval Error: {}", e))?;
    Ok(obj)
}


