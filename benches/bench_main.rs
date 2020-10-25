use criterion::criterion_main;

mod benchmarks;

criterion_main! {
    benchmarks::lexer::benches,
    benchmarks::parser::benches,
    benchmarks::evaluator::benches,
}