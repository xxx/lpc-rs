use std::sync::Arc;

use lpc_rs::{
    interpreter::{
        lpc_ref::LpcRef, process::Process, task::apply_function::apply_function_by_name, vm::Vm,
    },
    util::process_builder::ProcessCreator,
};
use lpc_rs_core::LpcIntInner;
use lpc_rs_errors::Result;
use lpc_rs_utils::config::Config;
use tokio::{sync::Barrier, task::JoinSet};

use crate::support::test_config_builder;

#[allow(dead_code)]
mod support;

pub async fn boot_vm(config: Config) -> Vm {
    Vm::new(config)
}

pub fn race_config(gil: bool) -> Config {
    test_config_builder()
        .max_execution_time(30_000_u64)
        .gil(gil)
        .build()
        .unwrap()
}

pub async fn spawn_applies(
    vm: &Vm,
    proc: Arc<Process>,
    func: &str,
    tasks: usize,
    iterations: usize,
) -> Vec<Result<LpcRef>> {
    let timeout = vm.global_state.config.max_execution_time;
    let template = vm.new_task_template();
    let func = func.to_owned();

    // Release every worker simultaneously to enforce contention and prevent an infinite hang waiting to join.
    let gate = Arc::new(Barrier::new(tasks));

    let mut set = JoinSet::new();

    for _ in 0..tasks {
        let proc = proc.clone();
        let template = template.clone();
        let func = func.clone();
        let gate = gate.clone();

        set.spawn(async move {
            gate.wait().await;

            let mut results = Vec::with_capacity(iterations);

            for _ in 0..iterations {
                let result = apply_function_by_name(
                    &func,
                    &[],
                    proc.clone(),
                    template.clone(),
                    Some(timeout),
                )
                .await
                .unwrap_or_else(|| panic!("no such function: `{func}`"));

                results.push(result);
            }

            results
        });
    }

    let mut all = Vec::with_capacity(tasks * iterations);

    while let Some(joined) = set.join_next().await {
        all.extend(joined.expect("a worker task panicked"));
    }

    all
}

/// Every apply must have succeeded. A test that "passes" because all of its workers
/// errored out is the exact failure this harness exists to catch, so call this before
/// asserting on any VM state.
fn assert_all_ok(results: &[Result<LpcRef>]) {
    assert!(!results.is_empty(), "no applies ran at all");

    for (i, result) in results.iter().enumerate() {
        if let Err(e) = result {
            e.emit_diagnostics();
            panic!("apply {i} of {} failed", results.len());
        }
    }
}

fn read_global(proc: &Process, index: usize) -> LpcRef {
    proc.globals
        .read()
        .get(index)
        .unwrap_or_else(|| panic!("no global at index {index}"))
        .clone()
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn lost_update_racy() {
    let counter = r#"
        int count = 0;

        void increment() {
            // do NOT change this to count++ - ++ is atomic and defeats the purpose
            count = count + 1;
        }
    "#;

    let vm = boot_vm(race_config(true)).await;
    let proc = vm
        .create_process_from_code("/counter.c", counter)
        .await
        .unwrap();
    let results = spawn_applies(&vm, proc.clone(), "increment", 8, 500).await;
    assert_all_ok(&results);

    let expected: LpcIntInner = 8 * 500;
    assert_eq!(read_global(&proc, 0), LpcRef::from(expected));
}
