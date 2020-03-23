use std::io;
use crate::lexer::Lexer;

pub fn start() {
    loop {
        let mut buf = String::new();
        match io::stdin().read_line(&mut buf) {
            Ok(_) => {
                let mut lexer = Lexer::new(buf.as_str());
                println!("{}", lexer.next_token());
            }
            Err(_) => panic!("read from STDIN failed")
        }
    }
}