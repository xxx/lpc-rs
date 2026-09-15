use std::sync::Arc;

use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use lpc_rs::interpreter::{
    task::{Task, task_template::TaskTemplate},
    vm::Vm,
};
use lpc_rs_utils::config::ConfigBuilder;

#[path = "support/profiler.rs"]
mod profiler;

const CHILD: &str = r#"
    int own = 4;
    function pointer;
    void create() { pointer = callback(); }
    int tick() { return own; }
    int qualified() {
        int sum;
        for (int i = 0; i < 2000; i++) sum += ::tick();
        return sum;
    }
    int pointers() {
        int sum;
        for (int i = 0; i < 2000; i++) sum += pointer();
        return sum;
    }
"#;

fn inheritance(c: &mut Criterion) {
    let runtime = tokio::runtime::Runtime::new().unwrap();
    let config = ConfigBuilder::default()
        .lib_dir("benches/fixtures/inheritance")
        .build()
        .unwrap();
    let vm = Vm::new(config);
    let template = TaskTemplate::from(vm.global_state.clone());
    let mut group = c.benchmark_group("inheritance");
    for (layout, parents) in [
        ("contiguous", "inherit \"/left\"; inherit \"/right\";"),
        ("reordered", "inherit \"/right\"; inherit \"/left\";"),
    ] {
        let process =
            runtime
                .block_on(vm.initialize_process_from_code(
                    format!("/{layout}.c"),
                    format!("{parents}{CHILD}"),
                ))
                .unwrap()
                .context
                .process;
        for name in ["read_cells", "references", "calls", "qualified", "pointers"] {
            let function = process.program.lookup_function(name).unwrap().clone();
            group.bench_function(BenchmarkId::new(layout, name), |b| {
                b.to_async(&runtime).iter(|| async {
                    let mut task: Task<64> =
                        Task::new(template.clone().into_task_context(Arc::clone(&process)));
                    task.timed_eval(function.clone(), &[], 0).await.unwrap();
                    std::hint::black_box(task.result());
                });
            });
        }
    }
    group.finish();
}

criterion_group! { name = benches; config = profiler::profiled(); targets = inheritance }
criterion_main!(benches);
