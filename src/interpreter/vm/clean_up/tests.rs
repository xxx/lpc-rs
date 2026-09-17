use lpc_rs_utils::config::ConfigBuilder;

use super::*;
use crate::{
    interpreter::{
        CommittedReader, lpc_ref::LpcRef, task::apply_function::apply_function_by_name, vm::Vm,
    },
    test_support::permissive_master,
};

fn vm(interval: u64) -> Vm {
    Vm::new(
        ConfigBuilder::default()
            .clean_up_interval(interval)
            .max_execution_time(0u64)
            .build()
            .unwrap(),
    )
}

async fn object(vm: &Vm, path: &str, code: &str) -> Arc<Process> {
    vm.initialize_process_from_code(path, code)
        .await
        .unwrap()
        .context
        .process
}

async fn call(vm: &Vm, process: &Arc<Process>, name: &str) -> lpc_rs_errors::Result<LpcRef> {
    apply_function_by_name(
        name,
        &[],
        process.clone(),
        TaskTemplate::from(vm.global_state.clone()),
        None,
    )
    .await
    .unwrap()
}

async fn advance(seconds: u64) {
    tokio::time::pause();
    tokio::time::advance(Duration::from_secs(seconds)).await;
    tokio::time::resume();
}

fn calls(vm: &Vm, process: &Arc<Process>) -> LpcRef {
    vm.global_state.committed_global(process, 0u16)
}

#[tokio::test]
async fn idle_delay_activity_and_repeated_queries() {
    let vm = vm(10);
    let process = object(
        &vm,
        "/room.c",
        "int calls; void visit() {} int clean_up(int refs) { calls++; return 1; }",
    )
    .await;
    vm.global_state.clean_up().await;
    assert_eq!(calls(&vm, &process), LpcRef::from(0));
    advance(9).await;
    call(&vm, &process, "visit").await.unwrap();
    advance(9).await;
    vm.global_state.clean_up().await;
    assert_eq!(calls(&vm, &process), LpcRef::from(0));
    advance(1).await;
    vm.global_state.clean_up().await;
    assert_eq!(calls(&vm, &process), LpcRef::from(1));
    vm.global_state.clean_up().await;
    assert_eq!(calls(&vm, &process), LpcRef::from(1));
    advance(10).await;
    vm.global_state.clean_up().await;
    assert_eq!(calls(&vm, &process), LpcRef::from(2));
}

#[tokio::test]
async fn opt_out_survives_gc_and_request_reenables_queries() {
    let vm = vm(10);
    let process = object(
        &vm,
        "/room.c",
        r#"
        int calls;
        int clean_up() { calls++; return 0; }
        int again() { return request_clean_up(); }
        void fail() { request_clean_up(); throw("abort"); }
        "#,
    )
    .await;
    assert_eq!(call(&vm, &process, "again").await.unwrap(), LpcRef::from(1));
    advance(10).await;
    vm.global_state.clean_up().await;
    assert_eq!(calls(&vm, &process), LpcRef::from(1));
    assert!(vm.global_state.gc().await.unwrap().is_ok());
    advance(10).await;
    vm.global_state.clean_up().await;
    assert_eq!(calls(&vm, &process), LpcRef::from(1));
    assert!(call(&vm, &process, "fail").await.is_err());
    advance(10).await;
    vm.global_state.clean_up().await;
    assert_eq!(calls(&vm, &process), LpcRef::from(1));
    assert_eq!(call(&vm, &process, "again").await.unwrap(), LpcRef::from(1));
    assert_eq!(call(&vm, &process, "again").await.unwrap(), LpcRef::from(1));
    vm.global_state.clean_up().await;
    assert_eq!(calls(&vm, &process), LpcRef::from(1));
    advance(10).await;
    vm.global_state.clean_up().await;
    assert_eq!(calls(&vm, &process), LpcRef::from(2));
}

#[tokio::test]
async fn void_return_opts_out_even_when_hook_requests_again() {
    let vm = vm(10);
    let process = object(
        &vm,
        "/room.c",
        "int calls; void clean_up() { calls++; request_clean_up(); }",
    )
    .await;
    advance(10).await;
    vm.global_state.clean_up().await;
    advance(10).await;
    vm.global_state.clean_up().await;
    assert_eq!(calls(&vm, &process), LpcRef::from(1));
}

#[tokio::test]
async fn disabled_scheduler_leaves_objects_alone() {
    let vm = vm(0);
    let process = object(
        &vm,
        "/room.c",
        "int calls; int clean_up() { calls++; return 1; } int again() { return request_clean_up(); }",
    )
    .await;
    assert_eq!(call(&vm, &process, "again").await.unwrap(), LpcRef::from(0));
    advance(10000).await;
    vm.global_state.clean_up().await;
    assert_eq!(calls(&vm, &process), LpcRef::from(0));
}

#[tokio::test]
async fn missing_hooks_uninitialized_and_system_objects_are_skipped() {
    let vm = Vm::new(
        ConfigBuilder::default()
            .clean_up_interval(10u64)
            .max_execution_time(0u64)
            .simul_efun_file("/simul.c")
            .build()
            .unwrap(),
    );
    let code = "int calls; int clean_up() { calls++; return 1; } int again() { return request_clean_up(); }";
    let master = object(&vm, "/secure/master.c", code).await;
    let simul = object(&vm, "/simul.c", code).await;
    let uninitialized = vm
        .global_state
        .object_space
        .create_process_from_code("/uninitialized.c", code)
        .await
        .unwrap();
    let plain = object(
        &vm,
        "/plain.c",
        "void create() { request_clean_up(); } int again() { return request_clean_up(); }",
    )
    .await;
    assert!(plain.cleanup.is_none());
    for process in [&master, &simul, &plain] {
        assert_eq!(call(&vm, process, "again").await.unwrap(), LpcRef::from(0));
    }
    advance(10).await;
    vm.global_state.clean_up().await;
    for process in [&master, &simul, &uninitialized] {
        assert_eq!(calls(&vm, process), LpcRef::from(0));
    }
}

#[tokio::test]
async fn resident_objects_and_clones_skip_automatic_cleanup_even_after_requesting_it() {
    let vm = vm(10);
    permissive_master(&vm.global_state.object_space).await;
    let resident = object(
        &vm,
        "/daemon.c",
        r#"
        #pragma resident
        int calls;
        int clean_up() { calls++; return 1; }
        void make() { clone_object("/daemon"); }
        int again() { return request_clean_up(); }
    "#,
    )
    .await;
    call(&vm, &resident, "make").await.unwrap();
    let clone = vm.global_state.object_space.lookup("/daemon#0").unwrap();
    let ordinary = object(
        &vm,
        "/room.c",
        "int calls; int clean_up() { calls++; return 1; }",
    )
    .await;

    advance(10).await;
    vm.global_state.clean_up().await;
    for process in [&resident, &clone] {
        assert_eq!(calls(&vm, process), LpcRef::from(0));
        assert_eq!(call(&vm, process, "again").await.unwrap(), LpcRef::from(0));
    }
    assert_eq!(calls(&vm, &ordinary), LpcRef::from(1));

    advance(10).await;
    vm.global_state.clean_up().await;
    for process in [&resident, &clone] {
        let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(
            TaskTemplate::from(vm.global_state.clone()).into_task_context(process.clone()),
        );
        task.timed_eval_seed(
            TaskSeed {
                process: process.clone(),
                entry: SeedEntry::CleanUp,
                args: vec![],
                initializes: false,
            },
            0,
        )
        .await
        .unwrap();
        assert_eq!(calls(&vm, process), LpcRef::from(0));
        call(&vm, process, "clean_up").await.unwrap();
        assert_eq!(calls(&vm, process), LpcRef::from(1));
    }
    assert_eq!(calls(&vm, &ordinary), LpcRef::from(2));
}

#[tokio::test]
async fn resident_objects_and_clones_can_be_explicitly_destructed() {
    let vm = vm(10);
    permissive_master(&vm.global_state.object_space).await;
    let resident = object(
        &vm,
        "/daemon.c",
        r#"
        #pragma resident
        int clean_up() { return 1; }
        void make() { clone_object("/daemon"); }
        void die() { destruct(this_object()); }
    "#,
    )
    .await;
    call(&vm, &resident, "make").await.unwrap();
    let clone = vm.global_state.object_space.lookup("/daemon#0").unwrap();
    call(&vm, &clone, "die").await.unwrap();
    call(&vm, &resident, "die").await.unwrap();
    assert!(vm.global_state.object_space.lookup("/daemon#0").is_none());
    assert!(vm.global_state.object_space.lookup("/daemon").is_none());
}

#[tokio::test]
async fn empty_room_destructs_but_occupied_room_survives() {
    let vm = vm(10);
    permissive_master(&vm.global_state.object_space).await;
    object(&vm, "/room.c", "int clean_up() { if (sizeof(all_inventory(this_object()))) return 1; destruct(this_object()); return 0; }").await;
    let occupant = object(&vm, "/occupant.c", "void enter() { move_object(find_object(\"/room\")); } void leave() { move_object(find_object(\"/outside\")); }").await;
    object(&vm, "/outside.c", "void create() {} ").await;
    call(&vm, &occupant, "enter").await.unwrap();
    advance(10).await;
    vm.global_state.clean_up().await;
    assert!(vm.global_state.object_space.lookup("/room").is_some());
    call(&vm, &occupant, "leave").await.unwrap();
    advance(10).await;
    vm.global_state.clean_up().await;
    assert!(vm.global_state.object_space.lookup("/room").is_none());
}

#[tokio::test]
async fn errors_roll_back_report_and_do_not_stop_other_objects() {
    let vm = vm(10);
    let master = object(
        &vm,
        "/secure/master.c",
        "int errors; void error_handler(mapping info) { errors++; }",
    )
    .await;
    let failing = object(
        &vm,
        "/fail.c",
        "int calls; int clean_up() { calls++; throw(\"cleanup failed\"); return 0; }",
    )
    .await;
    let good = object(
        &vm,
        "/good.c",
        "int calls; int clean_up() { calls++; return 1; }",
    )
    .await;
    advance(10).await;
    vm.global_state.clean_up().await;
    assert_eq!(calls(&vm, &failing), LpcRef::from(0));
    assert_eq!(calls(&vm, &good), LpcRef::from(1));
    assert_eq!(calls(&vm, &master), LpcRef::from(1));
    vm.global_state.clean_up().await;
    assert_eq!(calls(&vm, &master), LpcRef::from(1));
    advance(10).await;
    vm.global_state.clean_up().await;
    assert_eq!(calls(&vm, &master), LpcRef::from(2));
}

#[tokio::test]
async fn cleanup_cannot_bypass_destruct_authorization() {
    let vm = vm(10);
    let master = object(&vm, "/secure/master.c", "int errors; int valid_destruct() { return 0; } void error_handler(mapping info) { errors++; }").await;
    let room = object(
        &vm,
        "/room.c",
        "int calls; int clean_up() { calls++; destruct(this_object()); return 0; }",
    )
    .await;
    advance(10).await;
    vm.global_state.clean_up().await;
    assert!(vm.global_state.object_space.lookup("/room").is_some());
    assert_eq!(calls(&vm, &room), LpcRef::from(0));
    assert_eq!(calls(&vm, &master), LpcRef::from(1));
}

#[tokio::test]
async fn lease_prevents_overlap_and_releases_on_cancellation() {
    let vm = vm(10);
    let process = object(
        &vm,
        "/room.c",
        "int calls; int clean_up() { calls++; return 1; }",
    )
    .await;
    advance(10).await;
    let lease = Cleanup::claim(&process, Duration::from_secs(10)).unwrap();
    vm.global_state.clean_up().await;
    assert_eq!(calls(&vm, &process), LpcRef::from(0));
    drop(lease);
    vm.global_state.clean_up().await;
    assert_eq!(calls(&vm, &process), LpcRef::from(0));
    advance(10).await;
    vm.global_state.clean_up().await;
    assert_eq!(calls(&vm, &process), LpcRef::from(1));
}

#[tokio::test]
async fn clones_receive_zero_and_blueprint_counts_only_live_clones() {
    let vm = vm(10);
    permissive_master(&vm.global_state.object_space).await;
    let blueprint = object(&vm, "/room.c", "int references = -1; int clean_up(int refs) { references = refs; return 1; } void make() { clone_object(\"/room\"); } void die() { destruct(this_object()); }").await;
    call(&vm, &blueprint, "make").await.unwrap();
    let clone = vm.global_state.object_space.lookup("/room#0").unwrap();
    advance(10).await;
    vm.global_state.clean_up().await;
    assert_eq!(calls(&vm, &blueprint), LpcRef::from(2));
    assert_eq!(calls(&vm, &clone), LpcRef::from(0));
    call(&vm, &clone, "die").await.unwrap();
    advance(10).await;
    vm.global_state.clean_up().await;
    assert_eq!(calls(&vm, &blueprint), LpcRef::from(1));
}

#[tokio::test]
async fn stale_instance_is_skipped_after_its_path_is_reloaded() {
    let vm = vm(10);
    permissive_master(&vm.global_state.object_space).await;
    let code =
        "int calls; int clean_up() { calls++; return 1; } void die() { destruct(this_object()); }";
    let old = object(&vm, "/room.c", code).await;
    call(&vm, &old, "die").await.unwrap();
    let new = object(&vm, "/room.c", code).await;
    advance(10).await;
    vm.global_state
        .clean_up_object(old.clone(), Duration::from_secs(10))
        .await;
    assert_eq!(calls(&vm, &old), LpcRef::from(0));
    assert_eq!(calls(&vm, &new), LpcRef::from(0));
    vm.global_state.clean_up().await;
    assert_eq!(calls(&vm, &new), LpcRef::from(1));
}

#[tokio::test]
async fn vm_loop_schedules_cleanup_and_drains_it_on_shutdown() {
    let mut vm = Vm::new(
        ConfigBuilder::default()
            .clean_up_interval(1u64)
            .gc_interval(0u64)
            .build()
            .unwrap(),
    );
    object(
        &vm,
        "/secure/master.c",
        "int valid_shutdown() { return 1; }",
    )
    .await;
    object(&vm, "/room.c", "int clean_up() { shutdown(7); return 0; }").await;
    assert_eq!(
        tokio::time::timeout(Duration::from_secs(10), vm.run())
            .await
            .unwrap()
            .unwrap(),
        7
    );
}

#[tokio::test]
async fn cross_object_calls_refresh_the_receivers_idle_time() {
    let vm = vm(10);
    let room = object(
        &vm,
        "/room.c",
        "int calls; void visit() {} int clean_up() { calls++; return 1; }",
    )
    .await;
    let caller = object(
        &vm,
        "/caller.c",
        "void visit() { find_object(\"/room\")->visit(); }",
    )
    .await;
    advance(9).await;
    call(&vm, &caller, "visit").await.unwrap();
    advance(1).await;
    vm.global_state.clean_up().await;
    assert_eq!(calls(&vm, &room), LpcRef::from(0));
    advance(9).await;
    vm.global_state.clean_up().await;
    assert_eq!(calls(&vm, &room), LpcRef::from(1));
}

#[tokio::test]
async fn timeout_rolls_back_and_other_objects_still_get_queried() {
    let vm = Vm::new(
        ConfigBuilder::default()
            .clean_up_interval(10u64)
            .max_execution_time(100u64)
            .build()
            .unwrap(),
    );
    let master = object(
        &vm,
        "/secure/master.c",
        "int errors; void error_handler(mapping info) { errors++; }",
    )
    .await;
    let looping = object(
        &vm,
        "/loop.c",
        "int calls; int clean_up() { calls++; while (1) {} return 0; }",
    )
    .await;
    let good = object(
        &vm,
        "/good.c",
        "int calls; int clean_up() { calls++; return 1; }",
    )
    .await;
    advance(10).await;
    vm.global_state.clean_up().await;
    assert_eq!(calls(&vm, &looping), LpcRef::from(0));
    assert_eq!(calls(&vm, &good), LpcRef::from(1));
    assert_eq!(calls(&vm, &master), LpcRef::from(1));
}

#[tokio::test]
async fn inherited_cleanup_unloads_a_room_and_next_load_initializes_a_fresh_instance() {
    use crate::test_support::{PERMISSIVE_MASTER, lib_holding};

    let lib = lib_holding(
        "object-cleanup",
        &[
            ("secure/master.c", PERMISSIVE_MASTER),
            (
                "std/room.c",
                "int clean_up(int refs) { if (refs > 1 || sizeof(all_inventory(this_object()))) return 1; destruct(this_object()); return 0; }",
            ),
            (
                "room.c",
                "inherit \"/std/room\"; int *state = ({ 1, 2, 3 });",
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
    let loader = object(&vm, "/loader.c", "void enter() { load_object(\"/room\"); }").await;
    call(&vm, &loader, "enter").await.unwrap();
    let old = vm.global_state.object_space.lookup("/room").unwrap();
    advance(10).await;
    vm.global_state.clean_up().await;
    assert!(vm.global_state.object_space.lookup("/room").is_none());
    assert!(vm.global_state.gc().await.unwrap().unwrap().reclaimed > 0);
    call(&vm, &loader, "enter").await.unwrap();
    let new = vm.global_state.object_space.lookup("/room").unwrap();
    assert!(!Arc::ptr_eq(&old, &new));
    vm.global_state.clean_up().await;
    assert!(vm.global_state.object_space.lookup("/room").is_some());
}
