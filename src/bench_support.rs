//! The shared bench/test workload catalogue: one statement of each
//! measurement workload's LPC source, entry point, and volume.
//! Ungated: the bench targets build the lib without `cfg(test)`.

use std::sync::Arc;

use tokio::task::JoinSet;

use crate::interpreter::{
    process::Process,
    task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
    vm::Vm,
};

/// How a [`Workload`]'s objects are placed before measuring.
pub enum Kind {
    /// One object, compiled from `source` at `path`.
    Single {
        /// In-game path the object is created at.
        path: &'static str,
        /// LPC source compiled there.
        source: &'static str,
    },
    /// The `call_other` pair: a target object plus the caller that applies against it.
    Pair,
}

/// One measurement workload: its LPC source, entry point, and contention volume.
pub struct Workload {
    /// Criterion id in the contention and m0 groups.
    pub name: &'static str,
    /// Criterion id in `m1_task_cost`.
    pub task_label: &'static str,
    /// The function one apply calls.
    pub entry: &'static str,
    /// Fixed total applies for a contention block.
    pub total: usize,
    /// Object placement.
    pub kind: Kind,
}

/// Compute-bound, touches essentially no shared state.
pub static FIB: Workload = Workload {
    name: "fib",
    task_label: "fib10",
    entry: "run",
    total: 256,
    kind: Kind::Single {
        path: "/bench_fib.c",
        source: r#"
    int fib(int n) {
        if (n < 2) {
            return n;
        }
        return fib(n - 1) + fib(n - 2);
    }

    void run() {
        fib(10);
    }
"#,
    },
};

/// Maximal shared-state pressure for the work done: one global read-modify-write.
/// Does not use `++`, which compiles to an atomic op.
pub static COUNTER: Workload = Workload {
    name: "counter",
    task_label: "increment",
    entry: "increment",
    total: 8192,
    kind: Kind::Single {
        path: "/bench_counter.c",
        source: r#"
    int count = 0;

    void increment() {
        count = count + 1;
    }
"#,
    },
};

/// Realistic: a cross-object `call_other` pair, which is what most LPC actually does. Each
/// `call_other` builds a nested `Task`, so this exercises task setup, not just the eval loop.
pub static CALL_OTHER: Workload = Workload {
    name: "call_other",
    task_label: "call_other",
    entry: "bump",
    total: 2048,
    kind: Kind::Pair,
};

const CALLER_SOURCE: &str = r#"
    int total = 0;

    void bump() {
        object other = find_object("/bench_target");
        total = total + other->value();
    }
"#;

const TARGET_SOURCE: &str = r#"
    int value() {
        return 1;
    }
"#;

/// Array payload, light: the array analogue of `increment`. A small global array, a few
/// index reads and one index write per apply. This is the workload that actually touches
/// the payload container the persistent-payload swap changes; `fib`/`increment`/`call_other`
/// do not construct or index any array, so on their own they are a null measurement of the
/// payload's inner-loop cost.
pub static ARR_TOUCH: Workload = Workload {
    name: "arr_touch",
    task_label: "arr_touch",
    entry: "touch",
    total: 4096,
    kind: Kind::Single {
        path: "/bench_arr_touch.c",
        source: r#"
    int *a = ({ 1, 2, 3, 4 });

    void touch() {
        int x = a[0] + a[1] + a[2] + a[3];
        a[3] = x;
    }
"#,
    },
};

/// Array payload, heavy: read the whole array, then write a prefix of it, per apply.
/// Sizes 64 for the array and 8 for the write prefix keep it in the small-array regime
/// where a persistent structure's constant factors hurt most.
pub static ARR_CHURN: Workload = Workload {
    name: "arr_churn",
    task_label: "arr_churn",
    entry: "churn",
    total: 1024,
    kind: Kind::Single {
        path: "/bench_arr_churn.c",
        source: r#"
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
"#,
    },
};

/// Every workload, in report order.
pub static WORKLOADS: [&Workload; 5] = [&FIB, &COUNTER, &CALL_OTHER, &ARR_TOUCH, &ARR_CHURN];

/// Initialize `workload`'s object(s) on `vm` and hand back the apply target.
pub async fn setup_on(vm: &Vm, workload: &Workload) -> (Arc<Process>, TaskTemplate, u64) {
    let task = match &workload.kind {
        Kind::Single { path, source } => vm
            .initialize_process_from_code(path, source)
            .await
            .expect("the workload's object failed to initialize"),
        Kind::Pair => {
            vm.initialize_process_from_code("/bench_target.c", TARGET_SOURCE)
                .await
                .expect("the target object failed to initialize");
            vm.initialize_process_from_code("/bench_caller.c", CALLER_SOURCE)
                .await
                .expect("the caller object failed to initialize")
        }
    };
    let proc = task.context.process.clone();
    let template = TaskTemplate::from(vm.global_state.clone());
    let timeout = vm.global_state.config.max_execution_time;
    (proc, template, timeout)
}

/// Drive `workers` concurrent tasks, each applying `entry` `per_worker`
/// times; a failed apply or a panicked worker fails the caller.
pub async fn fan_out_applies(
    template: &TaskTemplate,
    proc: &Arc<Process>,
    entry: &str,
    workers: usize,
    per_worker: usize,
    timeout: u64,
) {
    let mut set = JoinSet::new();
    for _ in 0..workers {
        let proc = proc.clone();
        let template = template.clone();
        let entry = entry.to_owned();
        set.spawn(async move {
            for _ in 0..per_worker {
                apply_function_by_name(&entry, &[], proc.clone(), template.clone(), Some(timeout))
                    .await
                    .expect("the apply failed")
                    .expect("the apply returned an error");
            }
        });
    }
    while let Some(joined) = set.join_next().await {
        joined.expect("a worker panicked");
    }
}

#[cfg(test)]
mod tests {
    use lpc_rs_utils::config::ConfigBuilder;

    use super::*;

    #[tokio::test]
    async fn every_workload_sets_up_and_applies_once() {
        for workload in WORKLOADS {
            let config = ConfigBuilder::default()
                .lib_dir("./tests/fixtures/code")
                .max_execution_time(30_000_u64)
                .build()
                .unwrap();
            let vm = Vm::new(config);
            let (proc, template, timeout) = setup_on(&vm, workload).await;
            fan_out_applies(&template, &proc, workload.entry, 1, 1, timeout).await;
        }
    }
}
