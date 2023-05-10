use std::sync::Arc;

use criterion::{criterion_group, criterion_main, Criterion};
use lpc_rs::{
    compiler::Compiler,
    interpreter::{
        task::initialize_program::InitializeProgramBuilder, vm::global_state::GlobalState,
    },
};
use lpc_rs_utils::config::Config;

pub fn criterion_benchmark(c: &mut Criterion) {
    let code = r#"
        int fib(int n) {
            if (n < 2) {
                return n;
            }
            return fib(n - 1) + fib(n - 2);
        }

        int create() {
            int i = fib(20);
            return i;
        }
    "#;
    let runtime = tokio::runtime::Runtime::new().unwrap();

    let program = runtime
        .block_on(Compiler::default().compile_string("~/my_file.c", code))
        .expect("Failed to compile.");

    let program = Arc::new(program);
    let (tx, _rx) = tokio::sync::mpsc::channel(1024);
    let global_state = Arc::new(GlobalState::new(Config::default(), tx));

    c.bench_function("fib 20", |b| {
        b.to_async(&runtime).iter(|| async {
            let _ = InitializeProgramBuilder::<64>::default()
                .program(program.clone())
                .global_state(global_state.clone())
                .build()
                .await;
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
