mod token;
mod lexer;
mod ast;
mod parser;
mod evaluator;
mod repl;

fn main() {
    repl::start();
}
