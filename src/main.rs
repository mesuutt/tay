pub mod token;
pub mod lexer;
pub mod ast;
pub mod parser;
pub mod repl;

fn main() {
    repl::start();
}
