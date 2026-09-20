use std::sync::Arc;

use lpc_rs_errors::Result;
use lpc_rs_utils::config::ConfigBuilder;

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    interpreter::{
        lpc_ref::LpcRef,
        process::Process,
        stm::{AttemptBody, Transaction, TxnHandle, commit_changeset, start_txn},
        task::{
            SeedArg, SeedEntry, Task, TaskSeed, apply_function::apply_function_by_name,
            task_template::TaskTemplate,
        },
        vm::{
            Vm,
            object_update::{UpdateBody, UpdateRequest, UpdateStatus},
            vm_op::VmOp,
        },
    },
    test_support::{PERMISSIVE_MASTER, TempLib, lib_holding},
};

const SIMUL: &str = r#"
    int count = 10;
    int creates;
    void create() { creates++; }
    int version() { return count; }
    int bump() { return ++count; }
    int created() { return creates; }
    function capture(int ref value) { return (: ++value :); }
"#;

const ADMIN: &str = r#"
    object master;
    object simul;
    function named = &version();
    function local;
    function captured;
    void create() {
        master = find_object("/secure/master");
        simul = find_object("/secure/simul_efuns");
        local = &(simul)->version();
    }
    int request(mixed target) { return request_object_recompile(target); }
    mapping status(int id) { return query_object_recompile(id); }
    int identity() { return master == find_object("/secure/master") && simul == find_object("/secure/simul_efuns"); }
    int stale() { return !!catch(local()); }
    int named_call() { return named(); }
    int direct() { return version(); }
    int load() { return load_object("/consumer")->value(); }
    void clone_simul() { object clone = clone_object("/secure/simul_efuns"); clone->bump(); clone->bump(); clone->bump(); }
    void remember_capture() { captured = master->capture_global(); }
    int mutate_capture() { return captured(); }
    void arm() { simul->arm(); }
"#;

fn master(extra: &str) -> String {
    format!("{PERMISSIVE_MASTER}\nint valid_recompile() {{ return 1; }}\n{extra}")
}

async fn setup(name: &str, master: &str, simul: &str) -> (TempLib, Vm, Arc<Process>) {
    let root = lib_holding(
        name,
        &[("secure/master.c", master), ("secure/simul_efuns.c", simul)],
    );
    let config = ConfigBuilder::default()
        .lib_dir(root.to_str().unwrap())
        .simul_efun_file("/secure/simul_efuns")
        .build()
        .unwrap();
    let mut vm = Vm::new(config);
    vm.bootstrap().await.unwrap();
    let admin = vm
        .initialize_process_from_code("/admin.c", ADMIN)
        .await
        .unwrap()
        .context
        .process;
    (root, vm, admin)
}

async fn call(vm: &Vm, object: &Arc<Process>, name: &str, args: &[LpcRef]) -> Result<LpcRef> {
    apply_function_by_name(
        name,
        args,
        object.clone(),
        TaskTemplate::from(vm.global_state.clone()),
        Some(5000),
    )
    .await
    .expect("function exists")
}

async fn request(vm: &mut Vm, admin: &Arc<Process>, target: LpcRef) -> Arc<UpdateRequest> {
    let id = call(vm, admin, "request", &[target]).await.unwrap();
    let Some(VmOp::ObjectUpdate(request)) = vm.next_op() else {
        panic!("update queued");
    };
    assert_eq!(id, LpcRef::from(request.id));
    request
}

async fn run(vm: &mut Vm, admin: &Arc<Process>, target: LpcRef) -> UpdateStatus {
    let request = request(vm, admin, target).await;
    vm.global_state.run_object_update(request.clone()).await;
    vm.global_state.updates.get(request.id).unwrap()
}

#[tokio::test]
async fn paired_upgrade_preserves_identity_state_and_uses_new_exports() {
    let original = master(
        "int count = 30; int creates; void create() { creates++; enable_commands(); } int state() { return count + creates; }",
    );
    let (root, mut vm, admin) = setup("system-upgrade-pair", &original, SIMUL).await;
    let old_master = vm
        .global_state
        .object_space
        .lookup("/secure/master")
        .unwrap();
    let old_simul = vm
        .global_state
        .object_space
        .lookup("/secure/simul_efuns")
        .unwrap();
    call(&vm, &old_simul, "bump", &[]).await.unwrap();
    call(&vm, &admin, "clone_simul", &[]).await.unwrap();
    let clone = vm
        .global_state
        .object_space
        .lookup("/secure/simul_efuns#0")
        .unwrap();
    std::fs::write(
        root.join("secure/simul_efuns.c"),
        format!(
            "{} int added() {{ return 7; }}",
            SIMUL
                .replace("count = 10", "count = 900")
                .replace("return count;", "return count * 10;")
        ),
    )
    .unwrap();
    std::fs::write(root.join("secure/master.c"), master("int creates = 90; int answer = added() + bump(); int count = 800; void create() { creates += 1000; } int state() { return count + creates; } int result() { return answer; }")).unwrap();
    let status = run(&mut vm, &admin, "both".into()).await;
    assert_eq!(status.state, "succeeded", "{}", status.error);
    assert_eq!(status.updated, 3);
    assert_eq!(
        call(&vm, &clone, "version", &[]).await.unwrap(),
        LpcRef::from(130)
    );
    assert_eq!(
        call(&vm, &clone, "created", &[]).await.unwrap(),
        LpcRef::from(1)
    );
    assert_eq!(
        call(&vm, &admin, "identity", &[]).await.unwrap(),
        LpcRef::from(1)
    );
    assert_eq!(
        call(&vm, &old_master, "state", &[]).await.unwrap(),
        LpcRef::from(31)
    );
    assert_eq!(
        call(&vm, &old_master, "result", &[]).await.unwrap(),
        LpcRef::from(19)
    );
    assert_eq!(
        call(&vm, &old_simul, "created", &[]).await.unwrap(),
        LpcRef::from(1)
    );
    assert_eq!(
        call(&vm, &admin, "named_call", &[]).await.unwrap(),
        LpcRef::from(120)
    );
    assert_eq!(
        call(&vm, &admin, "stale", &[]).await.unwrap(),
        LpcRef::from(1)
    );
    std::fs::write(
        root.join("consumer.c"),
        "int value() { return added() + version(); }",
    )
    .unwrap();
    assert_eq!(
        call(&vm, &admin, "load", &[]).await.unwrap(),
        LpcRef::from(127)
    );
    vm.global_state.gc().await.unwrap().unwrap();
    assert_eq!(
        call(&vm, &old_master, "state", &[]).await.unwrap(),
        LpcRef::from(31)
    );
}

#[tokio::test]
async fn repeated_upgrades_validate_the_current_simul_exports() {
    let (root, mut vm, admin) = setup("system-upgrade-exports", &master(""), SIMUL).await;
    std::fs::write(
        root.join("secure/simul_efuns.c"),
        format!("{SIMUL} int added() {{ return 1; }}"),
    )
    .unwrap();
    assert_eq!(
        run(&mut vm, &admin, "simul_efun".into()).await.state,
        "succeeded"
    );
    std::fs::write(
        root.join("secure/simul_efuns.c"),
        format!("{SIMUL} int added() {{ return 2; }} int newest() {{ return 5; }}"),
    )
    .unwrap();
    let simul = vm
        .global_state
        .object_space
        .lookup("/secure/simul_efuns")
        .unwrap();
    assert_eq!(
        run(&mut vm, &admin, Arc::downgrade(&simul).into())
            .await
            .state,
        "succeeded"
    );
    std::fs::write(
        root.join("consumer.c"),
        "int value() { return added() + newest(); }",
    )
    .unwrap();
    assert_eq!(
        call(&vm, &admin, "load", &[]).await.unwrap(),
        LpcRef::from(7)
    );
    std::fs::write(root.join("secure/simul_efuns.c"), SIMUL).unwrap();
    let failed = run(&mut vm, &admin, "simul_efun".into()).await;
    assert_eq!(failed.state, "failed");
    assert!(failed.error.contains("was removed"), "{}", failed.error);
}

#[tokio::test]
async fn incompatible_simul_exports_refuse_publication_for_selectors_and_object_targets() {
    for (label, replacement) in [
        ("return", "string version() { return \"bad\"; }"),
        ("reference", "int version(int ref value) { return value; }"),
    ] {
        let (root, mut vm, admin) =
            setup(&format!("system-exports-{label}"), &master(""), SIMUL).await;
        let simul = vm
            .global_state
            .object_space
            .lookup("/secure/simul_efuns")
            .unwrap();
        std::fs::write(
            root.join("secure/simul_efuns.c"),
            SIMUL.replace("int version() { return count; }", replacement),
        )
        .unwrap();
        for target in [LpcRef::from("both"), Arc::downgrade(&simul).into()] {
            let status = run(&mut vm, &admin, target).await;
            assert_eq!(status.state, "failed");
            assert!(status.error.contains("incompatible"), "{}", status.error);
            assert_eq!(status.updated, 0);
            assert_eq!(
                call(&vm, &admin, "identity", &[]).await.unwrap(),
                LpcRef::from(1)
            );
            assert_eq!(
                call(&vm, &admin, "named_call", &[]).await.unwrap(),
                LpcRef::from(10)
            );
        }
    }
}

#[tokio::test]
async fn preparation_keeps_old_policy_code_globals_and_simul_state() {
    let original = master(
        r#"
        int writes;
        int permit = 17;
        int valid_write() {
            writes++;
            return permit == 17 && version() == 10 && this_object() == find_object("/secure/master");
        }
        int inspected() { return writes; }
    "#,
    );
    let (root, mut vm, admin) = setup("system-upgrade-authority", &original, SIMUL).await;
    std::fs::write(
        root.join("secure/simul_efuns.c"),
        SIMUL
            .replace("count = 10", "count = 400")
            .replace("return count;", "return count + 100;"),
    )
    .unwrap();
    std::fs::write(
        root.join("secure/master.c"),
        master(
            r#"
        int permit = 0;
        int writes = 999;
        int first = write_file("/permitted", "a");
        int second = write_file("/permitted", "b");
        int valid_write() { return 0; }
        int inspected() { return writes; }
    "#,
        ),
    )
    .unwrap();
    let status = run(&mut vm, &admin, "both".into()).await;
    assert_eq!(status.state, "succeeded", "{}", status.error);
    assert_eq!(
        std::fs::read_to_string(root.join("permitted")).unwrap(),
        "ab"
    );
    let current = vm
        .global_state
        .object_space
        .lookup("/secure/master")
        .unwrap();
    assert_eq!(
        call(&vm, &current, "inspected", &[]).await.unwrap(),
        LpcRef::from(2)
    );
    assert_eq!(
        call(&vm, &current, "valid_write", &[]).await.unwrap(),
        LpcRef::from(0)
    );
}

#[tokio::test]
async fn an_upgrade_cannot_grant_itself_permission_during_initialization() {
    let original = master("int permit; int valid_write() { return permit; }");
    let (root, mut vm, admin) = setup("system-upgrade-denial", &original, SIMUL).await;
    std::fs::write(root.join("secure/master.c"), master("int permit = 1; int side_effect = write_file(\"/forbidden\", \"bad\"); int valid_write() { return 1; }")).unwrap();
    let status = run(&mut vm, &admin, "master".into()).await;
    assert_eq!(status.state, "failed");
    assert!(!root.join("forbidden").exists());
    let current = vm
        .global_state
        .object_space
        .lookup("/secure/master")
        .unwrap();
    assert_eq!(
        call(&vm, &current, "valid_write", &[]).await.unwrap(),
        LpcRef::from(0)
    );
}

#[tokio::test]
async fn failure_after_staging_simul_discards_both_images_state_and_effects() {
    let original = master("int valid_write() { return 1; } int state() { return 1; }");
    let (root, mut vm, admin) = setup("system-upgrade-rollback", &original, SIMUL).await;
    std::fs::write(
        root.join("secure/simul_efuns.c"),
        format!(
            "{} int side_effect = write_file(\"/discarded\", \"bad\");",
            SIMUL.replace("return count;", "return 90;")
        ),
    )
    .unwrap();
    std::fs::write(root.join("secure/master.c"), master("int fail() { throw(\"failed master initializer\"); return 0; } int value = fail(); int state() { return 2; }")).unwrap();
    let status = run(&mut vm, &admin, "both".into()).await;
    assert_eq!(status.state, "failed");
    assert!(status.error.contains("failed master initializer"));
    assert!(!root.join("discarded").exists());
    assert_eq!(
        call(&vm, &admin, "identity", &[]).await.unwrap(),
        LpcRef::from(1)
    );
    assert_eq!(
        call(&vm, &admin, "named_call", &[]).await.unwrap(),
        LpcRef::from(10)
    );
    let current = vm
        .global_state
        .object_space
        .lookup("/secure/master")
        .unwrap();
    assert_eq!(
        call(&vm, &current, "state", &[]).await.unwrap(),
        LpcRef::from(1)
    );
}

#[tokio::test]
async fn object_target_preserves_global_cells_captured_by_unchanged_simul_code() {
    let original = master(
        "int count = 20; function capture_global() { return capture(ref count); } int state() { return count; }",
    );
    let (root, mut vm, admin) = setup("system-upgrade-ref", &original, SIMUL).await;
    call(&vm, &admin, "remember_capture", &[]).await.unwrap();
    let current = vm
        .global_state
        .object_space
        .lookup("/secure/master")
        .unwrap();
    std::fs::write(
        root.join("secure/master.c"),
        original.replace("count = 20", "count = 90"),
    )
    .unwrap();
    let status = run(&mut vm, &admin, Arc::downgrade(&current).into()).await;
    assert_eq!(status.state, "succeeded", "{}", status.error);
    assert_eq!(
        call(&vm, &admin, "mutate_capture", &[]).await.unwrap(),
        LpcRef::from(21)
    );
    assert_eq!(
        call(&vm, &current, "state", &[]).await.unwrap(),
        LpcRef::from(21)
    );
}

#[tokio::test]
async fn system_permission_is_required_for_selectors_and_object_targets_and_rechecked() {
    let policy = format!(
        "{PERMISSIVE_MASTER} int allow = 1; int valid_recompile() {{ return allow; }} void revoke() {{ allow = 0; }}"
    );
    let (_root, mut vm, admin) = setup("system-upgrade-permission", &policy, SIMUL).await;
    let queued = request(&mut vm, &admin, "master".into()).await;
    let current = vm
        .global_state
        .object_space
        .lookup("/secure/master")
        .unwrap();
    call(&vm, &current, "revoke", &[]).await.unwrap();
    vm.global_state.run_object_update(queued.clone()).await;
    assert_eq!(
        vm.global_state.updates.get(queued.id).unwrap().state,
        "failed"
    );
    for target in [LpcRef::from("master"), Arc::downgrade(&current).into()] {
        let error = call(&vm, &admin, "request", &[target]).await.unwrap_err();
        assert!(error.to_string().contains("permission denied"));
    }
}

#[tokio::test]
async fn concurrent_system_upgrades_conflict_and_retry() {
    let (root, mut vm, admin) = setup("system-upgrade-conflict", &master(""), SIMUL).await;
    let live = start_txn(&vm.global_state.committer_tx).await.unwrap();
    let txn = TxnHandle::new(Transaction::new(live.inner.clone()));
    let simul = vm
        .global_state
        .object_space
        .lookup("/secure/simul_efuns")
        .unwrap();
    let old = simul.image(&txn);
    std::fs::write(
        root.join("secure/simul_efuns.c"),
        SIMUL.replace("return count;", "return count + 100;"),
    )
    .unwrap();
    let queued = request(&mut vm, &admin, "simul_efun".into()).await;
    let gs = vm.global_state.clone();
    let mut body = UpdateBody {
        gs: &gs,
        request: &queued,
        txn: None,
        updated: 0,
    };
    let staged = body
        .begin_attempt(&vm.global_state.committer_tx)
        .await
        .unwrap()
        .unwrap();
    let concurrent = run(&mut vm, &admin, "simul_efun".into()).await;
    assert_eq!(concurrent.state, "succeeded", "{}", concurrent.error);
    assert!(
        body.commit_phase(&vm.global_state.committer_tx, staged)
            .await
            .unwrap()
            .0
            .is_err()
    );
    vm.global_state.run_object_update(queued.clone()).await;
    assert_eq!(
        vm.global_state.updates.get(queued.id).unwrap().state,
        "succeeded"
    );
    assert!(Arc::ptr_eq(&old, &simul.image(&txn)));
    assert_eq!(
        call(&vm, &admin, "named_call", &[]).await.unwrap(),
        LpcRef::from(110)
    );
    drop(live);
}

#[tokio::test]
async fn in_place_upgrade_invalidates_an_attempt_reading_the_old_image() {
    let (root, mut vm, admin) = setup("system-upgrade-image-conflict", &master(""), SIMUL).await;
    let live = start_txn(&vm.global_state.committer_tx).await.unwrap();
    let txn = TxnHandle::new(Transaction::new(live.inner.clone()));
    let simul = vm
        .global_state
        .object_space
        .lookup("/secure/simul_efuns")
        .unwrap();
    let old = simul.image(&txn);
    std::fs::write(
        root.join("secure/simul_efuns.c"),
        SIMUL.replace("return count;", "return count + 100;"),
    )
    .unwrap();
    assert_eq!(
        run(&mut vm, &admin, "simul_efun".into()).await.state,
        "succeeded"
    );
    assert!(Arc::ptr_eq(&old, &simul.image(&txn)));
    assert!(
        commit_changeset(
            &vm.global_state.committer_tx,
            txn.with(|t| t.take_changeset())
        )
        .await
        .unwrap()
        .is_err()
    );
    drop(live);
    assert_eq!(
        call(&vm, &admin, "named_call", &[]).await.unwrap(),
        LpcRef::from(110)
    );
}

#[tokio::test]
async fn system_callouts_survive_upgrade_and_their_local_pointers_become_stale() {
    let simul = format!("{SIMUL} void later() {{}} void arm() {{ call_out(later, 3600); }}");
    let (root, mut vm, admin) = setup("system-upgrade-callout", &master(""), &simul).await;
    call(&vm, &admin, "arm", &[]).await.unwrap();
    std::fs::write(
        root.join("secure/simul_efuns.c"),
        simul.replace("count = 10", "count = 90"),
    )
    .unwrap();
    let status = run(&mut vm, &admin, "simul_efun".into()).await;
    assert_eq!(status.state, "succeeded", "{}", status.error);
    assert_eq!(vm.global_state.with_call_outs(|calls| calls.len()), 1);
    assert_eq!(
        call(&vm, &admin, "stale", &[]).await.unwrap(),
        LpcRef::from(1)
    );
}

#[tokio::test]
async fn a_later_initializer_cannot_retire_an_earlier_member_of_the_pair() {
    let (root, mut vm, admin) = setup("system-upgrade-retirement", &master(""), SIMUL).await;
    std::fs::write(
        root.join("secure/master.c"),
        master(
            r#"
        int remove_simul() { destruct(find_object("/secure/simul_efuns")); return 1; }
        int removed = remove_simul();
    "#,
        ),
    )
    .unwrap();
    let status = run(&mut vm, &admin, "both".into()).await;
    assert_eq!(status.state, "failed");
    assert!(status.error.contains("retired"), "{}", status.error);
    assert_eq!(
        call(&vm, &admin, "identity", &[]).await.unwrap(),
        LpcRef::from(1)
    );
    assert_eq!(
        call(&vm, &admin, "named_call", &[]).await.unwrap(),
        LpcRef::from(10)
    );
}

#[tokio::test]
async fn each_system_prototype_requires_permission_for_selectors_and_object_arguments() {
    for allowed in ["master", "simul_efuns"] {
        let policy = format!(
            "{PERMISSIVE_MASTER} int valid_recompile(object prototype) {{ return prototype == find_object(\"/secure/{allowed}\"); }}"
        );
        let (_root, mut vm, admin) =
            setup(&format!("system-permission-{allowed}"), &policy, SIMUL).await;
        let denied = if allowed == "master" {
            "simul_efuns"
        } else {
            "master"
        };
        let object = vm
            .global_state
            .object_space
            .lookup(format!("/secure/{denied}"))
            .unwrap();
        let selector = if denied == "master" {
            "master"
        } else {
            "simul_efun"
        };
        for target in [
            LpcRef::from(selector),
            Arc::downgrade(&object).into(),
            "both".into(),
        ] {
            let error = call(&vm, &admin, "request", &[target]).await.unwrap_err();
            assert!(error.to_string().contains("permission denied"), "{error}");
            assert!(vm.next_op().is_none());
        }
    }
}

#[tokio::test]
async fn missing_permission_denies_system_selectors_and_object_arguments() {
    let (_root, mut vm, admin) = setup("system-missing-permission", PERMISSIVE_MASTER, SIMUL).await;
    let current = vm.global_state.object_space.master_object().unwrap();
    for target in [
        LpcRef::from("master"),
        "simul_efun".into(),
        "both".into(),
        Arc::downgrade(&current).into(),
    ] {
        let error = call(&vm, &admin, "request", &[target]).await.unwrap_err();
        assert!(error.to_string().contains("permission denied"), "{error}");
    }
    assert!(vm.next_op().is_none());
}

#[tokio::test]
async fn deferred_authorization_preserves_caller_program_and_command_giver() {
    let policy = format!(
        r#"{PERMISSIVE_MASTER}
        int valid_recompile(object prototype, object caller, string program) {{
            return prototype == this_object() && caller == find_object("/admin")
                && program == "/admin.c" && this_player() == caller;
        }}
    "#
    );
    let (_root, mut vm, admin) = setup("system-upgrade-provenance", &policy, SIMUL).await;
    let template = TaskTemplate::from(vm.global_state.clone());
    template.set_this_player(Some(admin.clone()));
    apply_function_by_name("request", &["master".into()], admin, template, Some(5000))
        .await
        .unwrap()
        .unwrap();
    let Some(VmOp::ObjectUpdate(request)) = vm.next_op() else {
        panic!("request queued");
    };
    vm.global_state.run_object_update(request.clone()).await;
    let status = vm.global_state.updates.get(request.id).unwrap();
    assert_eq!(status.state, "succeeded", "{}", status.error);
}

#[tokio::test]
async fn callbacks_prepared_before_upgrade_resolve_new_simul_functions() {
    let original = master("int valid_write() { return 1; }");
    let (root, mut vm, admin) = setup("system-upgrade-prepared", &original, SIMUL).await;
    let function =
        match crate::interpreter::CommittedReader::committed_global(&vm.global_state, &admin, 2) {
            LpcRef::Function(ptr) => ptr,
            _ => panic!("saved function"),
        };
    let prepared = vm
        .global_state
        .prepare_function_ptr(&function, &[], None)
        .await
        .unwrap()
        .unwrap();
    std::fs::write(
        root.join("secure/simul_efuns.c"),
        SIMUL.replace(
            "return count;",
            "write_file(\"/callback\", \"new\"); return count;",
        ),
    )
    .unwrap();
    assert_eq!(
        run(&mut vm, &admin, "simul_efun".into()).await.state,
        "succeeded"
    );
    prepared.execute(5000).await.unwrap();
    assert_eq!(
        std::fs::read_to_string(root.join("callback")).unwrap(),
        "new"
    );
}

#[tokio::test]
async fn an_existing_task_resolves_simul_and_master_at_attempt_start() {
    let (root, mut vm, admin) = setup(
        "system-upgrade-existing-task",
        &master("int policy() { return 1; }"),
        SIMUL,
    )
    .await;
    let mut task: Task<MAX_CALL_STACK_SIZE> =
        Task::new(TaskTemplate::from(vm.global_state.clone()).into_task_context(admin.clone()));
    let current = vm.global_state.object_space.master_object().unwrap();
    let mut master_task: Task<MAX_CALL_STACK_SIZE> =
        Task::new(TaskTemplate::from(vm.global_state.clone()).into_task_context(current.clone()));
    std::fs::write(
        root.join("secure/simul_efuns.c"),
        SIMUL.replace("return count;", "return 77;"),
    )
    .unwrap();
    std::fs::write(
        root.join("secure/master.c"),
        master("int policy() { return 88; }"),
    )
    .unwrap();
    assert_eq!(run(&mut vm, &admin, "both".into()).await.state, "succeeded");
    task.timed_eval(
        admin
            .initial_program()
            .lookup_function("direct")
            .unwrap()
            .clone(),
        &[],
        5000,
    )
    .await
    .unwrap();
    assert_eq!(task.result(), Some(LpcRef::from(77)));
    master_task
        .timed_eval_seed(
            TaskSeed {
                process: current,
                entry: SeedEntry::Master("policy".into()),
                args: Vec::<SeedArg>::new(),
                initializes: false,
            },
            5000,
        )
        .await
        .unwrap();
    assert_eq!(master_task.result(), Some(LpcRef::from(88)));
}

#[tokio::test]
async fn recursive_recompilation_fails_without_publication() {
    let (root, mut vm, admin) = setup("system-upgrade-recursive", &master(""), SIMUL).await;
    std::fs::write(
        root.join("secure/master.c"),
        master("int nested = request_object_recompile(\"both\");"),
    )
    .unwrap();
    let status = run(&mut vm, &admin, "both".into()).await;
    assert_eq!(status.state, "failed");
    assert!(
        status.error.contains("during preparation"),
        "{}",
        status.error
    );
    assert!(vm.next_op().is_none());
    assert_eq!(
        call(&vm, &admin, "named_call", &[]).await.unwrap(),
        LpcRef::from(10)
    );
}

#[tokio::test]
async fn missing_configuration_and_missing_resident_are_refused() {
    let root = lib_holding(
        "system-upgrade-missing-config",
        &[("secure/master.c", &master(""))],
    );
    let mut vm = Vm::new(crate::test_support::temp_lib_config(&root));
    vm.bootstrap().await.unwrap();
    let admin = vm
        .initialize_process_from_code(
            "/admin.c",
            "int request(string target) { return request_object_recompile(target); }",
        )
        .await
        .unwrap()
        .context
        .process;
    let error = call(&vm, &admin, "request", &["simul_efun".into()])
        .await
        .unwrap_err();
    assert!(error.to_string().contains("no simul-efun source"));
    assert!(vm.next_op().is_none());

    let (_root, mut vm, admin) = setup("system-upgrade-missing-resident", &master(""), SIMUL).await;
    let queued = request(&mut vm, &admin, "simul_efun".into()).await;
    vm.initialize_process_from_code(
        "/destroyer.c",
        "void create() { destruct(find_object(\"/secure/simul_efuns\")); }",
    )
    .await
    .unwrap();
    vm.global_state.run_object_update(queued.clone()).await;
    let status = vm.global_state.updates.get(queued.id).unwrap();
    assert_eq!(status.state, "failed");
    assert!(status.error.contains("no simul-efun object"));
}
