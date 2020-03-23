pub mod token;
pub mod lexer;
pub mod parser;
pub mod ast;
pub mod repl;

fn main() {
    repl::start();
}
