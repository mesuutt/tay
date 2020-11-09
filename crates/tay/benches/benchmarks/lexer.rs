use criterion::{Criterion, criterion_group};

use tay::lexer::{Lexer, Token};

pub fn bench_lexer(c: &mut Criterion) {
    let input = r#"fn fibonacci(x) { if (x == 0) {0} else {if (x == 1) {1} else {fibonacci(x - 1) + fibonacci(x - 2)}}}"#.to_string();

    c.bench_function("lexer fib", |b| b.iter(|| {
        let mut lex = Lexer::new(input.clone());
        loop {
            if let Token::EndOfFile = lex.next_token() {
                break;
            }
        }
    }));
}

criterion_group!(benches, bench_lexer);