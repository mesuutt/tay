use std::env;

use tay::evaluator::{Env, eval, Object};
use std::rc::Rc;
use std::cell::RefCell;
use tay::lexer::Lexer;
use tay::parser::Parser;

use rustyline::Editor;
use rustyline::error::ReadlineError;

const PROMPT: &str = ">> ";

pub(crate) fn start() {
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
