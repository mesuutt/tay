use criterion::{criterion_group, Criterion};
use tay::parser::Parser;
use tay::lexer::Lexer;

pub fn parse(c: &mut Criterion) {
    let input = r#"fn fibonacci(x) { if (x == 0) {0} else {if (x == 1) {1} else {fibonacci(x - 1) + fibonacci(x - 2)}}}"#.to_string();
    c.bench_function("parse fib", |b| b.iter(||
        Parser::new(Lexer::new(input.clone())).parse().unwrap())
    );
}

criterion_group!(benches, parse);
