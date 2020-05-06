use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::evaluator::{eval, Env, Object};
use rustyline::Editor;
use rustyline::error::ReadlineError;
use std::cell::RefCell;
use std::rc::Rc;
use std::env;

const PROMPT: &str = ">> ";

pub fn start() {
    let history_file_name = ".tay_history";
    println!(
        r#"
      ./|,,/|
     <   o o)  {name}
    <\ (    |  =======
   <\\  |\  |  Version: {version}
  <\\\  |(__)  History: ~/{history}
 <\\\\  |
"#,
        name = env!("CARGO_PKG_NAME"),
        version = env!("CARGO_PKG_VERSION"),
        history = history_file_name,
    );

    let mut editor = Editor::<()>::new();
    let history_path = format!("{}/{}", env::var("HOME").unwrap(), history_file_name);
    if editor.load_history(&history_path).is_err() {}
    let env= Rc::new(RefCell::new(Env::new()));

    loop {
        let readline = editor.readline(PROMPT);
        match readline {
            Ok(line) => {
                editor.add_history_entry(line.as_str());
                let program = Parser::new(Lexer::new(line)).parse();
                match program {
                   Ok(p) => {
                       match eval(p, env.clone()) {
                           Ok(obj) => {
                               match obj {
                                   Object::Null => {},
                                   _ => println!("{}", obj),
                               }
                           }
                           Err(e) => println!("EvalError: {}", e)
                       }
                   },
                    Err(e) => {
                        println!("ParseError: {}", e);
                        continue
                    }
                }

            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }

    editor.save_history(&history_path).unwrap();
}