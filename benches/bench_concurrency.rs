//! Run with:
//! ```text
//! cargo bench --bench bench_concurrency
//! ```

use std::sync::Arc;

use criterion::{BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};
use lpc_rs::{
    interpreter::{
        process::Process,
        task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
        vm::Vm,
    },
    util::process_builder::ProcessInitializer,
};
use lpc_rs_utils::config::ConfigBuilder;
use tokio::{runtime::Runtime, task::JoinSet};

/// Compute-bound, touches essentially no shared state.
const FIB: &str = r#"
    int fib(int n) {
        if (n < 2) {
            return n;
        }
        return fib(n - 1) + fib(n - 2);
    }

    void run() {
        fib(10);
    }
"#;

/// Maximal shared-state pressure for the work done: one global read-modify-write.
/// Does not use `++`, which compiles to an atomic op.
const COUNTER: &str = r#"
    int count = 0;

    void increment() {
        count = count + 1;
    }
"#;

/// Realistic: a cross-object `call_other` pair, which is what most LPC actually does. Each
/// `call_other` builds a nested `Task`, so this exercises task setup, not just the eval loop.
const CALLER: &str = r#"
    int total = 0;

    void bump() {
        object other = find_object("/bench_target");
        total = total + other->value();
    }
"#;

/// Array payload, light: the array analogue of `increment`. A small global array, a few
/// index reads and one index write per apply. This is the workload that actually touches
/// the payload container the persistent-payload swap changes; `fib`/`increment`/`call_other`
/// do not construct or index any array, so on their own they are a null measurement of the
/// payload's inner-loop cost.
const ARR_TOUCH: &str = r#"
    int *a = ({ 1, 2, 3, 4 });

    void touch() {
        int x = a[0] + a[1] + a[2] + a[3];
        a[3] = x;
    }
"#;

/// Array payload, heavy: read the whole array, then write a prefix of it, per apply.
/// Sizes 64 for the array and 8 for the write prefix keep it in the small-array regime
/// where a persistent structure's constant factors hurt most.
const ARR_CHURN: &str = r#"
    int *a = ({ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
               16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
               32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
               48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63 });

    void churn() {
        int i;
        int s = 0;
        for (i = 0; i < 64; i += 1) {
            s = s + a[i];
        }
        for (i = 0; i < 8; i += 1) {
            a[i] = s + i;
        }
    }
"#;

const TARGET: &str = r#"
    int value() {
        return 1;
    }
"#;

fn multi_thread_rt(threads: usize) -> Runtime {
    tokio::runtime::Builder::new_multi_thread()
        .worker_threads(threads)
        .enable_all()
        .build()
        .unwrap()
}

/// Boot a VM and initialize one object
async fn setup(gil: bool, path: &str, code: &str) -> (Vm, Arc<Process>, TaskTemplate, u64) {
    let config = ConfigBuilder::default()
        .lib_dir("./tests/fixtures/code")
        .max_execution_time(300_000_u64)
        .gil(gil)
        .build()
        .unwrap();

    let vm = Vm::new(config);
    let task = vm.initialize_process_from_code(path, code).await.unwrap();
    let proc = task.context.process.clone();
    let template = vm.new_task_template();
    let timeout = vm.global_state.config.max_execution_time;

    (vm, proc, template, timeout)
}

/// Include a second object for `call_other`
async fn setup_pair(gil: bool) -> (Vm, Arc<Process>, TaskTemplate, u64) {
    let (vm, _target, _t, _to) = setup(gil, "/bench_target.c", TARGET).await;

    let task = vm
        .initialize_process_from_code("/bench_caller.c", CALLER)
        .await
        .unwrap();
    let proc = task.context.process.clone();
    let template = vm.new_task_template();
    let timeout = vm.global_state.config.max_execution_time;

    (vm, proc, template, timeout)
}

/// Drive `workers` concurrent tokio tasks, each applying `func` `per_worker` times.
async fn run_workers(
    template: &TaskTemplate,
    proc: &Arc<Process>,
    func: &str,
    workers: usize,
    per_worker: usize,
    timeout: u64,
) {
    let mut set = JoinSet::new();

    for _ in 0..workers {
        let proc = proc.clone();
        let template = template.clone();
        let func = func.to_owned();

        set.spawn(async move {
            for _ in 0..per_worker {
                let _ = apply_function_by_name(
                    &func,
                    &[],
                    proc.clone(),
                    template.clone(),
                    Some(timeout),
                )
                .await;
            }
        });
    }

    while set.join_next().await.is_some() {}
}

/// M0: fixed total work, varying worker count. Throughput is reported per apply, so a
/// perfectly scaling VM shows a flat time-per-element as workers increase.
fn m0_scaling(c: &mut Criterion) {
    // Per-workload work counts. Only the *shape* of each curve matters, so the counts need not match.
    for (name, path, code, func, total) in [
        ("m0_scaling_fib", "/bench_fib.c", FIB, "run", 256usize),
        (
            "m0_scaling_counter",
            "/bench_counter.c",
            COUNTER,
            "increment",
            8192usize,
        ),
    ] {
        let mut group = c.benchmark_group(name);
        group.throughput(Throughput::Elements(total as u64));
        group.sample_size(20);

        for workers in [1usize, 2, 4, 8, 16] {
            let rt = multi_thread_rt(workers.max(1));
            let (_vm, proc, template, timeout) = rt.block_on(setup(false, path, code));
            let per_worker = total / workers;

            group.bench_with_input(
                BenchmarkId::from_parameter(workers),
                &workers,
                |b, &workers| {
                    b.to_async(&rt)
                        .iter(|| run_workers(&template, &proc, func, workers, per_worker, timeout));
                },
            );
        }

        group.finish();
    }
}

/// M1: cost of a single task, no concurrency.
/// Increment is the shortest realistic task.
fn m1_task_cost(c: &mut Criterion) {
    let mut group = c.benchmark_group("m1_task_cost");
    let rt = multi_thread_rt(1);

    for (label, path, code, func) in [
        ("fib10", "/bench_fib.c", FIB, "run"),
        ("increment", "/bench_counter.c", COUNTER, "increment"),
        ("arr_touch", "/bench_arr_touch.c", ARR_TOUCH, "touch"),
        ("arr_churn", "/bench_arr_churn.c", ARR_CHURN, "churn"),
    ] {
        let (_vm, proc, template, timeout) = rt.block_on(setup(false, path, code));

        group.bench_function(label, |b| {
            b.to_async(&rt).iter(|| {
                apply_function_by_name(func, &[], proc.clone(), template.clone(), Some(timeout))
            });
        });
    }

    let (_vm, proc, template, timeout) = rt.block_on(setup_pair(false));
    group.bench_function("call_other", |b| {
        b.to_async(&rt).iter(|| {
            apply_function_by_name("bump", &[], proc.clone(), template.clone(), Some(timeout))
        });
    });

    group.finish();
}

/// M3: GIL serialization cost. GIL serializes whole tasks; single committer serializes just the commit.
fn m3_gil(c: &mut Criterion) {
    const TOTAL: usize = 8192;
    const WORKERS: usize = 8;

    let mut group = c.benchmark_group("m3_gil");
    group.throughput(Throughput::Elements(TOTAL as u64));
    group.sample_size(20);

    for (label, gil) in [("off", false), ("on", true)] {
        let rt = multi_thread_rt(WORKERS);
        let (_vm, proc, template, timeout) = rt.block_on(setup(gil, "/bench_counter.c", COUNTER));

        group.bench_function(label, |b| {
            b.to_async(&rt).iter(|| {
                run_workers(
                    &template,
                    &proc,
                    "increment",
                    WORKERS,
                    TOTAL / WORKERS,
                    timeout,
                )
            });
        });
    }

    group.finish();
}

criterion_group!(benches, m0_scaling, m1_task_cost, m3_gil);
criterion_main!(benches);
