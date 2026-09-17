use std::time::Duration;

use lpc_rs_utils::config::ConfigBuilder;

use super::*;
use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    interpreter::{CommittedReader, task::apply_function::apply_function_by_name, vm::Vm},
};

#[tokio::test(start_paused = true)]
async fn arrival_rejects_stale_destruction_and_retry_preserves_room() {
    use crate::test_support::{PERMISSIVE_MASTER, lib_holding};
    let lib = lib_holding(
        "cleanup-arrival",
        &[
            ("secure/master.c", PERMISSIVE_MASTER),
            (
                "room.c",
                "int calls; int clean_up() { calls++; if (sizeof(all_inventory(this_object()))) return 1; destruct(this_object()); return 0; }",
            ),
            (
                "visitor.c",
                "void enter() { move_object(find_object(\"/room\")); }",
            ),
        ],
    );
    let mut vm = Vm::new(
        ConfigBuilder::default()
            .lib_dir(lib.to_str().unwrap())
            .clean_up_interval(10u64)
            .max_execution_time(0u64)
            .build()
            .unwrap(),
    );
    vm.bootstrap().await.unwrap();
    vm.initialize_process_from_code(
        "/loader.c",
        "void create() { load_object(\"/room\"); load_object(\"/visitor\"); }",
    )
    .await
    .unwrap();
    let room = vm.global_state.object_space.lookup("/room").unwrap();
    let visitor = vm.global_state.object_space.lookup("/visitor").unwrap();
    tokio::time::advance(Duration::from_secs(10)).await;
    let template = task_template::TaskTemplate::from(vm.global_state.clone());
    let mut task: Task<MAX_CALL_STACK_SIZE> =
        Task::new(template.clone().into_task_context(room.clone()));
    task.seed = Some(TaskSeed {
        process: room.clone(),
        entry: SeedEntry::CleanUp,
        args: vec![],
        initializes: false,
    });
    let tx = &vm.global_state.committer_tx;
    let mut live = task.begin_attempt(tx).await.unwrap().unwrap();
    assert!(!room.is_live(&task.context.txn));
    apply_function_by_name("enter", &[], visitor, template, None)
        .await
        .unwrap()
        .unwrap();
    live.disarm();
    let (commit, _) = task.commit_phase(tx, live).await.unwrap();
    assert!(commit.is_err());
    let mut live = task.begin_attempt(tx).await.unwrap().unwrap();
    assert!(room.is_live(&task.context.txn));
    live.disarm();
    let (commit, effects) = task.commit_phase(tx, live).await.unwrap();
    assert!(commit.is_ok());
    task.deliver(effects).await.unwrap();
    assert!(vm.global_state.object_space.lookup("/room").is_some());
    assert_eq!(
        vm.global_state.committed_global(&room, 0u16),
        LpcRef::from(1)
    );
}

#[tokio::test(start_paused = true)]
async fn activity_between_attempts_skips_retry() {
    let vm = Vm::new(
        ConfigBuilder::default()
            .clean_up_interval(10u64)
            .max_execution_time(0u64)
            .build()
            .unwrap(),
    );
    let room = vm
        .initialize_process_from_code(
            "/room.c",
            "int calls; void visit() { calls = 5; } int clean_up() { calls = calls * 2 + 1; return 0; }",
        )
        .await
        .unwrap()
        .context
        .process;
    tokio::time::advance(Duration::from_secs(10)).await;
    let template = task_template::TaskTemplate::from(vm.global_state.clone());
    let mut task: Task<MAX_CALL_STACK_SIZE> =
        Task::new(template.clone().into_task_context(room.clone()));
    task.seed = Some(TaskSeed {
        process: room.clone(),
        entry: SeedEntry::CleanUp,
        args: vec![],
        initializes: false,
    });
    let tx = &vm.global_state.committer_tx;
    let mut live = task.begin_attempt(tx).await.unwrap().unwrap();
    apply_function_by_name("visit", &[], room.clone(), template, None)
        .await
        .unwrap()
        .unwrap();
    live.disarm();
    assert!(task.commit_phase(tx, live).await.unwrap().0.is_err());
    let mut live = task.begin_attempt(tx).await.unwrap().unwrap();
    live.disarm();
    assert!(task.commit_phase(tx, live).await.unwrap().0.is_ok());
    assert!(task.result().is_none());
    assert_eq!(
        vm.global_state.committed_global(&room, 0u16),
        LpcRef::from(5)
    );
    tokio::time::advance(Duration::from_secs(10)).await;
    let seed = task.seed.clone().unwrap();
    task.timed_eval_seed(seed, 0).await.unwrap();
    assert_eq!(
        vm.global_state.committed_global(&room, 0u16),
        LpcRef::from(11)
    );
}
