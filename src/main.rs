pub mod token;
pub mod lexer;
pub mod repl;
pub mod ast;

fn main() {
    repl::start();
}
