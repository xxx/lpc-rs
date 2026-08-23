use std::sync::Arc;

use criterion::{Criterion, criterion_group, criterion_main};
use lpc_rs::{
    compiler::Compiler,
    interpreter::{
        process::Process, task::task_template::TaskTemplate, vm::global_state::GlobalState,
    },
    util::process_builder::process_insert_and_initialize_program,
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
            let _ = process_insert_and_initialize_program::<64>(
                Arc::new(Process::new(program.clone())),
                TaskTemplate::from(global_state.clone()),
            )
            .await;
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
