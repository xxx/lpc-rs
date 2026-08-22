use std::sync::Arc;

use indoc::indoc;
use lpc_rs::{
    interpreter::{
        CommittedReader,
        lpc_ref::LpcRef,
        process::Process,
        task::apply_function::apply_function_by_name,
        vm::{Vm, global_state::GlobalState},
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

pub fn race_config() -> Config {
    test_config_builder()
        .max_execution_time(30_000_u64)
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

fn read_global(gs: &Arc<GlobalState>, proc: &Process, index: usize) -> LpcRef {
    gs.committed_global(proc, index as u16)
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

    let vm = boot_vm(race_config()).await;
    let proc = vm
        .create_process_from_code("/counter.c", counter)
        .await
        .unwrap();
    let results = spawn_applies(&vm, proc.clone(), "increment", 8, 500).await;
    assert_all_ok(&results);

    let expected: LpcIntInner = 8 * 500;
    assert_eq!(
        read_global(&vm.global_state, &proc, 0),
        LpcRef::from(expected)
    );
}

/// A read-modify-write that
/// crosses two `call_other` boundaries must be atomic. Pre-join
/// (per-sub-task transactions) `query_count` and `set_count` are separate
/// transactions → lost updates. Post-join the whole `bump` is one
/// transaction → the committer's conflict rule converges to exactly
/// `tasks * iterations`.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn call_other_cross_object_rmw_is_atomic() {
    let counter = r#"
        int count = 0;
        int query_count() { return count; }
        void set_count(int c) { count = c; }
    "#;
    let bump = r#"
        void bump() {
            object counter = find_object("/counter");
            int c = counter->query_count();
            counter->set_count(c + 1);
        }
    "#;
    let vm = boot_vm(race_config()).await;
    let counter_proc = vm
        .create_process_from_code("/counter.c", counter)
        .await
        .unwrap();
    let bump_proc = vm.create_process_from_code("/bump.c", bump).await.unwrap();

    let results = spawn_applies(&vm, bump_proc, "bump", 8, 500).await;
    assert_all_ok(&results);

    let expected: LpcIntInner = 8 * 500;
    assert_eq!(
        read_global(&vm.global_state, &counter_proc, 0),
        LpcRef::from(expected)
    );
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn aliased_array_torn_read_racy() {
    let code = r#"
        int *pair = ({ 10, 0 });

        void shift() {
            pair[0] = pair[0] - 1;
            pair[1] = pair[1] + 1;
        }

        void unshift() {
            pair[0] = pair[0] + 1;
            pair[1] = pair[1] - 1;
        }

        int sum() {
            return pair[0] + pair[1];
        }
    "#;

    let vm = boot_vm(race_config()).await;
    let proc = vm
        .initialize_process_from_code("/arr.c", code)
        .await
        .unwrap()
        .context
        .process
        .clone();

    let (writes, reads) = tokio::join!(
        spawn_applies(&vm, proc.clone(), "shift", 4, 500),
        spawn_applies(&vm, proc.clone(), "sum", 4, 500),
    );

    assert_all_ok(&writes);
    assert_all_ok(&reads);

    let ten = LpcRef::from(10);
    let torn = reads.iter().filter(|r| r.as_ref().unwrap() != &ten).count();
    assert_eq!(torn, 0, "{torn} of {} readers saw a torn pair", reads.len());
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn multithread_sync_racy() {
    let room = indoc! { r#"
        int weight = 0;

        void set_weight(int w) {
            weight = w;
        }

        int query_weight() {
            return weight;
        }
    "# };

    let mover = indoc! { r#"
        int weight = 10;

        void move(object new_env) {
            object old_env = environment();
            if (old_env) {
                old_env->set_weight(old_env->query_weight() - weight);
            }

            move_object(new_env);
            new_env->set_weight(new_env->query_weight() + weight);
        }
    "# };

    let runner = indoc! { r#"
        void run() {
            object room1 = find_object("/room1");
            object room2 = find_object("/room2");

            object mover1 = find_object("/mover1");
            object mover2 = find_object("/mover2");

            int i = 50;
            while(i--) {
                mover1->move(room2);
                mover1->move(room1);

                mover2->move(room1);
                mover2->move(room2);
            }

        }
    "# };

    let config = race_config();

    let vm = Vm::new(config);
    let room1_proc = vm
        .initialize_process_from_code("/room1.c", room)
        .await
        .unwrap();
    let room2_proc = vm
        .initialize_process_from_code("/room2.c", room)
        .await
        .unwrap();

    let _mover1_proc = vm.initialize_process_from_code("/mover1.c", mover).await;
    let _mover2_proc = vm.initialize_process_from_code("/mover2.c", mover).await;

    let runner = vm
        .initialize_process_from_code("/runner1.c", runner)
        .await
        .unwrap()
        .context
        .process
        .clone();

    let results = spawn_applies(&vm, runner, "run", 4, 10).await;

    assert_all_ok(&results);

    let room1 = room1_proc.context.process;
    let room2 = room2_proc.context.process;

    let room1_weight = vm.global_state.committed_global(&room1, 0u16);
    let room2_weight = vm.global_state.committed_global(&room2, 0u16);

    // println!("room1: {}", room1_weight);
    // for item in room1.position.inventory_iter().collect_vec() {
    //     println!("room1 item: {}", item);
    // }
    //
    // println!("room2: {}", room2_weight);
    // for item in room2.position.inventory_iter().collect_vec() {
    //     println!("room2 item: {}", item);
    // }
    //
    assert_eq!(room1_weight, LpcRef::from(10));
    assert_eq!(room2_weight, LpcRef::from(10));
}
