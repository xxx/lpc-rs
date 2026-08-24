//! Run with:
//! ```text
//! cargo bench --bench bench_concurrency
//! ```

use std::sync::Arc;

use criterion::{BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};
use lpc_rs::{
    bench_support::{self as workloads, COUNTER, FIB, WORKLOADS},
    interpreter::{CommitterStats, task::apply_function::apply_function_by_name, vm::Vm},
};
use lpc_rs_utils::config::ConfigBuilder;
use tokio::runtime::Runtime;

#[path = "support/profiler.rs"]
mod profiler;

fn multi_thread_rt(threads: usize) -> Runtime {
    tokio::runtime::Builder::new_multi_thread()
        .worker_threads(threads)
        .enable_all()
        .build()
        .unwrap()
}

/// A VM against the bench fixture config.
async fn bench_vm() -> Arc<Vm> {
    let config = ConfigBuilder::default()
        .lib_dir("./tests/fixtures/code")
        .max_execution_time(300_000_u64)
        .build()
        .unwrap();
    Arc::new(Vm::new(config))
}

/// Contention: fixed total work per (workload, workers); prints the committer's conflict rate beside criterion's throughput.
fn contention(c: &mut Criterion) {
    const WORKERS: [usize; 3] = [1, 4, 8];

    let mut group = c.benchmark_group("contention");
    group.sample_size(10);

    // Criterion list mode runs the group body to register IDs but skips the bench closures.
    // Keep setup and stats out of that path: their stdout breaks nextest's test listing.
    let listing = std::env::args().any(|arg| arg == "--list");

    for workload in WORKLOADS {
        let name = workload.name;
        for &workers in &WORKERS {
            if listing {
                // Register the same ID the real path registers, with a no-op body.
                group.bench_with_input(BenchmarkId::new(name, workers), &workers, |_, _| {});
                continue;
            }

            let rt = multi_thread_rt(workers);
            // Fresh Vm per block, so a before/after stats diff is unambiguous; per-sample printing inside
            // the closure would interleave criterion's stderr headers and misattribute the deltas.
            let vm = rt.block_on(bench_vm());
            let (proc, template, timeout) = rt.block_on(workloads::setup_on(&vm, workload));
            let per_worker = workload.total / workers;
            let before: CommitterStats = rt.block_on(vm.global_state.committer_stats()).unwrap();
            let t_before = vm.global_state.attempt_telemetry();

            group.bench_with_input(BenchmarkId::new(name, workers), &workers, |b, &workers| {
                b.to_async(&rt).iter(|| {
                    workloads::fan_out_applies(
                        &template,
                        &proc,
                        workload.entry,
                        workers,
                        per_worker,
                        timeout,
                    )
                });
            });

            let after: CommitterStats = rt.block_on(vm.global_state.committer_stats()).unwrap();
            let t_after = vm.global_state.attempt_telemetry();
            let commits = after.commits.saturating_sub(before.commits);
            let conflicts = after.conflicts.saturating_sub(before.conflicts);
            let reply_failures = after.reply_failures.saturating_sub(before.reply_failures);
            let busy_ms = (after.busy_ns.saturating_sub(before.busy_ns)) as f64 / 1e6;
            let q_peak = after.queue_peak;
            let rate = if commits > 0 {
                conflicts as f64 / commits as f64
            } else {
                0.0
            };
            let applies = t_after.applies.saturating_sub(t_before.applies);
            let attempts = t_after.attempts.saturating_sub(t_before.attempts);
            let attempts_per_apply = if applies > 0 {
                attempts as f64 / applies as f64
            } else {
                0.0
            };
            let yield_ms = t_after
                .backoff_yield
                .saturating_sub(t_before.backoff_yield)
                .as_secs_f64()
                * 1e3;
            let sleep_ms = t_after
                .backoff_sleep
                .saturating_sub(t_before.backoff_sleep)
                .as_secs_f64()
                * 1e3;
            let sleep_req_ms = t_after
                .backoff_sleep_requested
                .saturating_sub(t_before.backoff_sleep_requested)
                .as_secs_f64()
                * 1e3;
            println!(
                "[{name} w={workers}] commits={commits} conflicts={conflicts} reply_fail={reply_failures} rate={rate:.4} attempts_per_apply={attempts_per_apply:.3} backoff_yield_ms={yield_ms:.1} backoff_sleep_ms={sleep_ms:.1} backoff_sleep_req_ms={sleep_req_ms:.1} committer_busy_ms={busy_ms:.1} q_peak={q_peak}"
            );
        }
    }

    group.finish();
}

/// M0: fixed total work, varying worker count. Throughput is reported per apply, so a
/// perfectly scaling VM shows a flat time-per-element as workers increase.
fn m0_scaling(c: &mut Criterion) {
    // Per-workload work counts. Only the *shape* of each curve matters, so the counts need not match.
    for (workload, name, total) in [
        (&FIB, "m0_scaling_fib", 256usize),
        (&COUNTER, "m0_scaling_counter", 8192usize),
    ] {
        let mut group = c.benchmark_group(name);
        group.throughput(Throughput::Elements(total as u64));
        group.sample_size(20);

        for workers in [1usize, 2, 4, 8, 16] {
            let rt = multi_thread_rt(workers.max(1));
            let vm = rt.block_on(bench_vm());
            let (proc, template, timeout) = rt.block_on(workloads::setup_on(&vm, workload));
            let per_worker = total / workers;

            group.bench_with_input(
                BenchmarkId::from_parameter(workers),
                &workers,
                |b, &workers| {
                    b.to_async(&rt).iter(|| {
                        workloads::fan_out_applies(
                            &template,
                            &proc,
                            workload.entry,
                            workers,
                            per_worker,
                            timeout,
                        )
                    });
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

    for workload in WORKLOADS {
        let vm = rt.block_on(bench_vm());
        let (proc, template, timeout) = rt.block_on(workloads::setup_on(&vm, workload));

        #[cfg(feature = "opcode-profile")]
        let ops_before = lpc_rs::interpreter::opcode_profile::snapshot();
        group.bench_function(workload.task_label, |b| {
            b.to_async(&rt).iter(|| {
                apply_function_by_name(
                    workload.entry,
                    &[],
                    proc.clone(),
                    template.clone(),
                    Some(timeout),
                )
            });
        });
        #[cfg(feature = "opcode-profile")]
        print_opcode_shares(workload.task_label, &ops_before);
    }

    group.finish();
}

criterion_group! {
    name = benches;
    config = profiler::profiled();
    targets = m0_scaling, m1_task_cost, contention
}
criterion_main!(benches);

/// Top opcodes dispatched since `before`, as counts and shares.
#[cfg(feature = "opcode-profile")]
fn print_opcode_shares(label: &str, before: &lpc_rs::interpreter::opcode_profile::Snapshot) {
    use lpc_rs_asm::instruction::Instruction;

    let after = lpc_rs::interpreter::opcode_profile::snapshot();
    let mut deltas: Vec<(usize, u64)> = after
        .iter()
        .zip(before.iter())
        .map(|(a, b)| a.saturating_sub(*b))
        .enumerate()
        .collect();
    let total: u64 = deltas.iter().map(|(_, d)| d).sum();
    if total == 0 {
        return;
    }
    deltas.sort_by_key(|&(_, d)| std::cmp::Reverse(d));
    let line = deltas
        .iter()
        .take(8)
        .map(|&(i, d)| {
            format!(
                "{}={} ({:.1}%)",
                Instruction::MNEMONICS[i],
                d,
                d as f64 * 100.0 / total as f64
            )
        })
        .collect::<Vec<_>>()
        .join(" ");
    println!("[opcodes:{label}] total={total} {line}");
}
