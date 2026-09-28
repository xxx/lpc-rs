use std::time::{Duration, Instant};

use criterion::{Criterion, Throughput, criterion_group, criterion_main};
use lpc_rs::interpreter::{
    lpc_ref::LpcRef,
    task::{apply_function::apply_function, task_template::TaskTemplate},
    vm::Vm,
};
use lpc_rs_utils::config::ConfigBuilder;

#[path = "support/profiler.rs"]
mod profiler;

const SOURCE: &str = r#"
int stored;
int sum(int n) {
    int sum;
    for (int i = 0; i < n; i++) sum += i;
    return sum;
}
int local_copy(int n) {
    int value;
    for (int i = 0; i < n; i++) value = i;
    return value;
}
int global_copy(int n) {
    for (int i = 0; i < n; i++) stored = i;
    return stored + 1;
}
int global_add(int n) {
    for (int i = 0; i < n; i++) stored = i + 1;
    return stored - 1;
}
int upvalue_copy(int n) {
    int value;
    function work = (: [int count] {
        for (int i = 0; i < count; i++) value = i;
        return value;
    } :);
    return work(n);
}
int upvalue_add(int n) {
    int value;
    function work = (: [int count] {
        for (int i = 0; i < count; i++) value = i + 1;
        return value - 1;
    } :);
    return work(n);
}
int foreach_one(int n) {
    int *items = ({ 1 });
    int sum;
    for (int i = 0; i < n; i++) foreach (int value : items) sum += value;
    return sum;
}
int foreach_eight(int n) {
    int *items = ({ 1, 2, 3, 4, 5, 6, 7, 8 });
    int sum;
    for (int i = 0; i < n; i++) foreach (int value : items) sum += value;
    return sum;
}
int foreach_literal(int n) {
    int sum;
    for (int i = 0; i < n; i++) foreach (int value : ({ 1, 2 })) sum += value;
    return sum;
}
"#;

fn integer_execution(c: &mut Criterion) {
    const ITERATIONS: u64 = 100_000;
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();
    let config = ConfigBuilder::default()
        .lib_dir("./tests/fixtures/code")
        .build()
        .unwrap();
    let vm = Vm::new(config);
    let task = rt
        .block_on(vm.initialize_process_from_code("/arithmetic.c", SOURCE))
        .unwrap();
    let process = task.context.process().clone();
    drop(task);
    let mut group = c.benchmark_group("integer_execution");
    group.throughput(Throughput::Elements(ITERATIONS));

    for (name, expected) in [
        ("sum", (ITERATIONS * (ITERATIONS - 1) / 2) as i64),
        ("local_copy", ITERATIONS as i64 - 1),
        ("global_copy", ITERATIONS as i64),
        ("global_add", ITERATIONS as i64 - 1),
        ("upvalue_copy", ITERATIONS as i64 - 1),
        ("upvalue_add", ITERATIONS as i64 - 1),
        ("foreach_one", ITERATIONS as i64),
        ("foreach_eight", ITERATIONS as i64 * 36),
        ("foreach_literal", ITERATIONS as i64 * 3),
    ] {
        let function = process
            .initial_program()
            .lookup_function(name)
            .unwrap()
            .clone();
        let args = [LpcRef::from(ITERATIONS as i64)];
        let run = || async {
            apply_function(
                function.clone(),
                &args,
                TaskTemplate::from(vm.global_state.clone()).into_task_context(process.clone()),
                None,
            )
            .await
            .unwrap()
        };
        assert_eq!(rt.block_on(run()), LpcRef::from(expected));
        group.bench_function(name, |b| {
            let run = &run;
            let state = &vm.global_state;
            b.to_async(&rt).iter_custom(|iterations| async move {
                let mut elapsed = Duration::ZERO;
                for _ in 0..iterations {
                    let start = Instant::now();
                    std::hint::black_box(run().await);
                    elapsed += start.elapsed();
                    // Reclaim captured cells outside the timed interval.
                    state.gc().await.unwrap().unwrap();
                }
                elapsed
            });
        });
    }
    group.finish();
}

criterion_group! {
    name = benches;
    config = profiler::profiled();
    targets = integer_execution
}
criterion_main!(benches);
