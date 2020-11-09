mod repl;
mod file;

use std::process;
use std::env;

fn main() {
    if let Some(filename) = env::args().nth(1) {
        if let Err(e) = file::eval_file(&filename) {
            println!("{}", e);
            process::exit(1);
        }
    } else {
        repl::start();
    }
}


