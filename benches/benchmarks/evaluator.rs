use criterion::{criterion_group, Criterion};
use tay::parser::Parser;
use tay::lexer::Lexer;
use tay::evaluator::{Env, eval};
use std::rc::Rc;
use std::cell::RefCell;

pub fn bench_evaluator(c: &mut Criterion) {
    let input = r#"fn fibonacci(x) { if (x == 0) {0} else {if (x == 1) {1} else {fibonacci(x - 1) + fibonacci(x - 2)}}}; fibonacci(2)"#.to_string();
    let program = Parser::new(Lexer::new(input.clone())).parse().unwrap();
    c.bench_function("eval fib", |b| b.iter(|| {
        let _ = eval(program.clone(), Rc::new(RefCell::new(Env::new())));
    }));
}

criterion_group!(benches, bench_evaluator);
