//! Measure simul-efun registry contention with setup and GC outside timing.
//!
//! Run `cargo run --release --example living_registry_contention -- --samples 3`.
//! List options accept comma-separated values: `--names 128,1024 --workers 1,4,8
//! --writes 0,1,10 --work 0 --variants scan,indexed,sharded`; `--operations 4000`
//! sets total completed transactions per case and must be a multiple of 100 times
//! the worker count.

use std::{
    sync::Arc,
    time::{Duration, Instant},
};

use lpc_rs::interpreter::{
    lpc_ref::LpcRef,
    process::Process,
    task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
    vm::Vm,
};
use lpc_rs_utils::config::ConfigBuilder;
use tokio::{sync::Barrier, task::JoinSet};

const TIMEOUT: u64 = 300_000;

#[derive(Clone, Copy)]
struct Case<'a> {
    variant: &'a str,
    names: usize,
    workers: usize,
    writes: usize,
    work: usize,
}

struct Batch {
    wall: Duration,
    reads: Vec<Duration>,
    writes: Vec<Duration>,
}

async fn apply(
    template: &TaskTemplate,
    process: &Arc<Process>,
    entry: &str,
    args: &[LpcRef],
) -> i64 {
    let result = apply_function_by_name(
        entry,
        args,
        process.clone(),
        template.clone(),
        Some(TIMEOUT),
    )
    .await
    .expect("fixture entry exists")
    .unwrap_or_else(|error| panic!("{entry}: {error}"));
    match result {
        LpcRef::Int(value) => value.0,
        value => panic!("{entry} returned {value:?}, expected an integer"),
    }
}

async fn batch(
    case: Case<'_>,
    template: &TaskTemplate,
    process: &Arc<Process>,
    operations: usize,
) -> Batch {
    let barrier = Arc::new(Barrier::new(case.workers + 1));
    let mut jobs = JoinSet::new();
    for worker in 0..case.workers {
        let template = template.clone();
        let process = process.clone();
        let barrier = barrier.clone();
        let per_worker = operations / case.workers;
        let names = case.names;
        let writes = case.writes;
        let work = case.work;
        jobs.spawn(async move {
            let mut read_times = Vec::with_capacity(per_worker);
            let mut write_times = Vec::with_capacity(per_worker * writes / 100);
            barrier.wait().await;
            for iteration in 0..per_worker {
                // Every 100 operations has exactly the requested mix, staggered across workers.
                let write = (iteration * 37 + worker * 17) % 100 < writes;
                let index = (iteration * 73 + worker * 997) % names;
                let args = if write {
                    vec![LpcRef::from(worker as i64)]
                } else {
                    vec![LpcRef::from(index as i64), LpcRef::from(work as i64)]
                };
                let started = Instant::now();
                let value = apply(
                    &template,
                    &process,
                    if write { "rename" } else { "lookup" },
                    &args,
                )
                .await;
                let elapsed = started.elapsed();
                if write {
                    assert!(value > 0);
                    write_times.push(elapsed);
                } else {
                    assert_eq!(value, 1, "lookup result or work checksum was incorrect");
                    read_times.push(elapsed);
                }
            }
            (read_times, write_times)
        });
    }
    let started = Instant::now();
    barrier.wait().await;
    let mut reads = Vec::with_capacity(operations);
    let mut writes = Vec::new();
    while let Some(joined) = jobs.join_next().await {
        let (reader, writer) = joined.expect("worker panicked");
        reads.extend(reader);
        writes.extend(writer);
    }
    Batch {
        wall: started.elapsed(),
        reads,
        writes,
    }
}

fn percentile(times: &[Duration], percent: usize) -> f64 {
    if times.is_empty() {
        return 0.0;
    }
    times[(times.len() * percent).div_ceil(100).saturating_sub(1)].as_secs_f64() * 1e6
}

async fn measure(case: Case<'_>, sample: usize, operations: usize) {
    let config = ConfigBuilder::default()
        .lib_dir("./examples/fixtures/living_registry")
        .simul_efun_file(format!("/{}.c", case.variant))
        .max_execution_time(TIMEOUT)
        .build()
        .expect("fixture config");
    let vm = Vm::new(config);
    vm.initialize_simul_efuns()
        .await
        .expect("configured simul-efuns")
        .expect("initialize registry");
    vm.initialize_process_from_code("/actor.c", include_str!("fixtures/living_registry/actor.c"))
        .await
        .expect("initialize actor");
    let process = vm
        .initialize_process_from_code(
            "/driver.c",
            include_str!("fixtures/living_registry/driver.c"),
        )
        .await
        .expect("initialize driver")
        .context
        .process;
    let template = TaskTemplate::from(vm.global_state.clone());
    apply(
        &template,
        &process,
        "populate",
        &[
            LpcRef::from(case.names as i64),
            LpcRef::from(case.workers as i64),
        ],
    )
    .await;
    let warmup = batch(case, &template, &process, 800).await;
    let initial = apply(&template, &process, "verify", &[]).await;
    assert_eq!(initial as usize, warmup.writes.len());
    vm.global_state.gc().await.unwrap().unwrap();

    let before = vm.global_state.committer_stats().await.unwrap();
    let telemetry = vm.global_state.attempt_telemetry();
    let mut result = batch(case, &template, &process, operations).await;
    let after = vm.global_state.committer_stats().await.unwrap();
    let measured = vm.global_state.attempt_telemetry();
    let completed = measured.owning_tasks - telemetry.owning_tasks;
    let attempts = measured.owning_attempts - telemetry.owning_attempts;
    let conflicts = after.conflicts - before.conflicts;
    assert_eq!(completed as usize, operations);
    assert_eq!(attempts, completed + conflicts as u64);
    assert_eq!(measured.errors - telemetry.errors, 0);
    assert_eq!(after.reply_failures - before.reply_failures, 0);
    assert_eq!(result.writes.len(), operations * case.writes / 100);
    assert_eq!(
        apply(&template, &process, "verify", &[]).await,
        initial + result.writes.len() as i64,
        "renames were lost or applied twice"
    );
    if case.writes == 0 || case.workers == 1 {
        assert_eq!(conflicts, 0);
    }
    result.reads.sort_unstable();
    result.writes.sort_unstable();
    println!(
        "{},{},{},{},{},{},{},{:.6},{:.1},{},{:.4},{:.3},{:.3},{:.3},{:.3},{:.3},{:.3},{:.3},{:.3},{}",
        case.variant,
        case.names,
        case.workers,
        case.writes,
        case.work,
        sample,
        operations,
        result.wall.as_secs_f64(),
        operations as f64 / result.wall.as_secs_f64(),
        conflicts,
        attempts as f64 / completed as f64,
        percentile(&result.reads, 50),
        percentile(&result.reads, 95),
        percentile(&result.reads, 99),
        percentile(&result.writes, 50),
        percentile(&result.writes, 95),
        percentile(&result.writes, 99),
        (after.commit_service_ns - before.commit_service_ns) as f64 / 1e6,
        (measured.admission_wait - telemetry.admission_wait).as_secs_f64() * 1e3,
        after.validation_scanned_versions - before.validation_scanned_versions,
    );
}

fn option(name: &str, default: &str) -> String {
    let args: Vec<_> = std::env::args().collect();
    args.windows(2)
        .find(|pair| pair[0] == name)
        .map_or_else(|| default.to_owned(), |pair| pair[1].clone())
}

fn numbers(name: &str, default: &str) -> Vec<usize> {
    option(name, default)
        .split(',')
        .map(|value| value.parse().expect("numeric option"))
        .collect()
}

fn main() {
    let samples: usize = option("--samples", "3").parse().unwrap();
    let operations: usize = option("--operations", "4000").parse().unwrap();
    let variants = option("--variants", "scan,indexed,sharded");
    let mut cases = Vec::new();
    for names in numbers("--names", "128,1024") {
        for workers in numbers("--workers", "1,4,8") {
            assert!(names > 0 && workers > 0);
            assert!(operations > 0 && operations.is_multiple_of(100 * workers));
            assert!(800usize.is_multiple_of(100 * workers));
            for writes in numbers("--writes", "0,1,10") {
                assert!(writes <= 100);
                for work in numbers("--work", "0") {
                    for variant in variants.split(',') {
                        assert!(matches!(variant, "scan" | "indexed" | "sharded"));
                        cases.push(Case {
                            variant,
                            names,
                            workers,
                            writes,
                            work,
                        });
                    }
                }
            }
        }
    }
    println!(
        "variant,names,workers,write_percent,work,sample,operations,seconds,ops_per_second,conflicts,attempts_per_op,read_p50_us,read_p95_us,read_p99_us,write_p50_us,write_p95_us,write_p99_us,commit_service_ms,admission_wait_ms,validation_versions"
    );
    for sample in 0..samples {
        let mut order: Vec<_> = (0..cases.len()).collect();
        fastrand::Rng::with_seed(42 + sample as u64).shuffle(&mut order);
        for index in order {
            let case = cases[index];
            eprintln!(
                "{} names={} workers={} writes={}% work={} sample={sample}",
                case.variant, case.names, case.workers, case.writes, case.work
            );
            let runtime = tokio::runtime::Builder::new_multi_thread()
                .worker_threads(case.workers)
                .enable_all()
                .build()
                .unwrap();
            runtime.block_on(measure(case, sample, operations));
        }
    }
}
