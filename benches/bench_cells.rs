use std::sync::Arc;

use criterion::{Criterion, criterion_group, criterion_main};
use lpc_rs::{
    compiler::Compiler,
    interpreter::{
        process::Process, program::Program, task::task_template::TaskTemplate,
        vm::global_state::GlobalState,
    },
    util::process_builder::process_insert_and_initialize_program,
};
use lpc_rs_utils::config::Config;

#[path = "support/profiler.rs"]
mod profiler;

/// One loop of 20k iterations per case, so the per-access cost of a
/// register, a global cell and a captured cell can be compared.
const CASES: &[(&str, &str)] = &[
    (
        "register add",
        "void create() { int i; int k = 1; int a; for (i = 0; i < 20000; i++) a = a + k; }",
    ),
    (
        "global read",
        "int g = 1; void create() { int i; int a; for (i = 0; i < 20000; i++) a = a + g; }",
    ),
    (
        "global read+write",
        "int g; void create() { int i; for (i = 0; i < 20000; i++) g = g + 1; }",
    ),
    (
        "captured read",
        "void create() { int k = 1; function f = (: int i; int a; for (i = 0; i < 20000; i++) a = a + k; :); f(); }",
    ),
    (
        "captured read+write",
        "void create() { int k; function f = (: int i; for (i = 0; i < 20000; i++) k = k + 1; :); f(); }",
    ),
];

pub fn criterion_benchmark(c: &mut Criterion) {
    let runtime = tokio::runtime::Runtime::new().unwrap();
    let (tx, _rx) = tokio::sync::mpsc::channel(1024);
    let global_state = Arc::new(GlobalState::new(Config::default(), tx));

    let mut group = c.benchmark_group("cells");
    for (name, code) in CASES {
        let program: Arc<Program> = Arc::new(
            runtime
                .block_on(Compiler::default().compile_string("~/cells.c", code))
                .expect("Failed to compile."),
        );
        group.bench_function(*name, |b| {
            b.to_async(&runtime).iter(|| async {
                let _ = process_insert_and_initialize_program::<64>(
                    Arc::new(Process::new(program.clone())),
                    TaskTemplate::from(global_state.clone()),
                )
                .await;
            })
        });
    }
    group.finish();
}

criterion_group! {
    name = benches;
    config = profiler::profiled();
    targets = criterion_benchmark
}
criterion_main!(benches);
