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
    int typed_array(int n) {
        int *a = allocate(n);
        for (int i = 0; i < n; i++) a[i] = i;
        return a[n - 1];
    }
    int mixed_array(int n) {
        mixed a = allocate(n);
        for (int i = 0; i < n; i++) a[i] = i;
        return a[n - 1];
    }
    int typed_mapping(int n) {
        mapping a = ([]);
        for (int i = 0; i < n; i++) a[i] = i;
        return a[n - 1];
    }
    int mixed_mapping(int n) {
        mixed a = ([]);
        for (int i = 0; i < n; i++) a[i] = i;
        return a[n - 1];
    }
    int captured_array(int n) {
        mixed *rows = ({allocate(n)});
        for (int i = 0; i < n; i++) rows[0][i] = i;
        return rows[0][n - 1];
    }
    int captured_mapping(int n) {
        mixed *rows = ({([])});
        for (int i = 0; i < n; i++) rows[0][i] = i;
        return rows[0][n - 1];
    }
    int string_character(int n) {
        string text = "abcdefgh";
        for (int i = 0; i < n; i++) text[3] = 'x';
        return text[3];
    }
    int unicode_character(int n) {
        mixed text = "😀😀😀😀";
        for (int i = 0; i < n; i++) text[2] = 'x';
        return text[2];
    }
    int changing_string_character(int n) {
        string text = "a" * 64;
        for (int i = 0; i < n; i++) text[32] = 'x' + (i % 2);
        return text[32];
    }
    int changing_unicode_character(int n) {
        mixed text = "😀" * 16;
        for (int i = 0; i < n; i++) text[8] = 'é' + (i % 2);
        return text[8];
    }
"#;

fn indexed_stores(c: &mut Criterion) {
    const WRITES: u64 = 100_000;
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
        .block_on(vm.initialize_process_from_code("/index_assignment.c", SOURCE))
        .unwrap();
    let process = task.context.process().clone();
    drop(task);
    let mut group = c.benchmark_group("indexed_stores");
    group.throughput(Throughput::Elements(WRITES));

    for (name, expected) in [
        ("typed_array", WRITES as i64 - 1),
        ("mixed_array", WRITES as i64 - 1),
        ("typed_mapping", WRITES as i64 - 1),
        ("mixed_mapping", WRITES as i64 - 1),
        ("captured_array", WRITES as i64 - 1),
        ("captured_mapping", WRITES as i64 - 1),
        ("string_character", 120),
        ("unicode_character", 120),
        ("changing_string_character", 121),
        ("changing_unicode_character", 234),
    ] {
        let function = process
            .initial_program()
            .lookup_function(name)
            .unwrap()
            .clone();
        let args = [LpcRef::from(WRITES as i64)];
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
                    // Reclaim each call's containers outside the timed interval.
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
    targets = indexed_stores
}
criterion_main!(benches);
