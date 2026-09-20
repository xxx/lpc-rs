use super::*;
use crate::interpreter::{
    lpc_ref::LpcRef,
    stm::{Effect, commit_changeset},
};
use crate::interpreter::{
    stm::{AttemptBody, Transaction, TxnHandle},
    task::task_template::TaskTemplate,
    vm::object_update::{UpdateBody, UpdateRequest, UpdateStatus},
};
use crate::{
    interpreter::{
        stm::start_txn,
        task::{
            SeedArg, SeedEntry, TaskSeed,
            apply_function::{applied_by_name, apply_function_by_name, apply_function_in_master},
        },
        vm::{Vm, vm_op::VmOp},
    },
    test_support::{PERMISSIVE_MASTER, TempLib, lib_holding},
};
use lpc_rs_utils::config::ConfigBuilder;

const SIMUL: &str = "int count = 10; int version() { return ++count; }";
const USER: &str = r#"
    function saved = &version();
    object original;
    function local;
    int request(string which) { return request_system_reload(which); }
    mapping status(int id) { return query_system_reload(id); }
    int direct() { return version(); }
    int pointer() { return saved(); }
    void remember() {
        original = find_object("/secure/simul_efuns");
        local = &(original)->version();
    }
    int stale() { return !objectp(original) && !!catch(local()); }
"#;

fn master(extra: &str) -> String {
    format!(
        "{PERMISSIVE_MASTER}\nint valid_reload(string target, object caller, string program) {{ return 1; }}\nint valid_write() {{ return 1; }}\n{extra}"
    )
}

async fn setup(name: &str) -> (TempLib, Vm, Arc<Process>) {
    let root = lib_holding(
        name,
        &[
            ("secure/simul_efuns.c", SIMUL),
            ("secure/master.c", &master("int policy() { return 1; }")),
        ],
    );
    let config = ConfigBuilder::default()
        .lib_dir(root.to_str().unwrap())
        .simul_efun_file("/secure/simul_efuns")
        .build()
        .unwrap();
    let mut vm = Vm::new(config);
    vm.bootstrap().await.unwrap();
    let user = vm
        .initialize_process_from_code("/admin.c", USER)
        .await
        .unwrap()
        .context
        .process;
    (root, vm, user)
}

async fn call(vm: &Vm, user: &Arc<Process>, name: &str, args: &[LpcRef]) -> Result<LpcRef> {
    apply_function_by_name(
        name,
        args,
        user.clone(),
        TaskTemplate::from(vm.global_state.clone()),
        Some(5000),
    )
    .await
    .expect("function exists")
}

async fn request(vm: &mut Vm, user: &Arc<Process>, target: &str) -> Arc<UpdateRequest> {
    let id = call(vm, user, "request", &[target.into()]).await.unwrap();
    let Some(VmOp::ObjectUpdate(request)) = vm.next_op() else {
        panic!("reload queued");
    };
    assert_eq!(id, LpcRef::from(request.id));
    request
}

async fn run(vm: &mut Vm, user: &Arc<Process>, target: &str) -> UpdateStatus {
    let request = request(vm, user, target).await;
    vm.global_state.run_object_update(request.clone()).await;
    vm.global_state.updates.get(request.id).unwrap()
}

#[tokio::test]
async fn simul_reload_refreshes_existing_calls_pointers_and_globals() {
    let (root, mut vm, user) = setup("system-reload-simul").await;
    assert_eq!(
        call(&vm, &user, "direct", &[]).await.unwrap(),
        LpcRef::from(11)
    );
    call(&vm, &user, "remember", &[]).await.unwrap();
    std::fs::write(
        root.join("secure/simul_efuns.c"),
        "int count = 40; int version() { return ++count; }",
    )
    .unwrap();
    let status = run(&mut vm, &user, "simul_efun").await;
    assert_eq!(status.state, "succeeded", "{}", status.error);
    assert_eq!(
        call(&vm, &user, "direct", &[]).await.unwrap(),
        LpcRef::from(41)
    );
    assert_eq!(
        call(&vm, &user, "pointer", &[]).await.unwrap(),
        LpcRef::from(42)
    );
    assert_eq!(
        call(&vm, &user, "stale", &[]).await.unwrap(),
        LpcRef::from(1)
    );
    let result = applied_by_name(
        "status",
        &[status.request.id.into()],
        user,
        TaskTemplate::from(vm.global_state.clone()),
        Some(5000),
    )
    .await
    .unwrap()
    .unwrap();
    assert_eq!(
        result.mapping().unwrap().get(&LpcRef::from("state")),
        Some(&LpcRef::from("succeeded"))
    );
}

#[tokio::test]
async fn paired_reload_compiles_master_against_staged_exports_and_initializes_once() {
    let (root, mut vm, user) = setup("system-reload-pair").await;
    std::fs::write(
        root.join("secure/simul_efuns.c"),
        "int version() { return 20; } int added() { return 22; }",
    )
    .unwrap();
    std::fs::write(root.join("secure/master.c"), master("int answer; void create() { answer = version() + added(); write_file(\"/created\", \"once\"); } int policy() { return answer; }")).unwrap();
    let status = run(&mut vm, &user, "both").await;
    assert_eq!(status.state, "succeeded", "{}", status.error);
    let value = apply_function_in_master(
        "policy",
        &[],
        TaskTemplate::from(vm.global_state.clone()),
        Some(5000),
    )
    .await
    .unwrap()
    .unwrap();
    assert_eq!(value, LpcRef::from(42));
    assert_eq!(
        std::fs::read_to_string(root.join("created")).unwrap(),
        "once"
    );
}

#[tokio::test]
async fn preparation_failures_preserve_both_objects_and_discard_effects() {
    for (label, simul, replacement_master, message) in [
        (
            "syntax",
            "int version() { return ; invalid }",
            master(""),
            "",
        ),
        (
            "removed",
            "int other() { return 1; }",
            master(""),
            "was removed",
        ),
        (
            "return",
            "string version() { return \"bad\"; }",
            master(""),
            "incompatible",
        ),
        (
            "reference",
            "int version(int ref x) { return x; }",
            master(""),
            "incompatible",
        ),
        (
            "initialize",
            "int version() { return 20; } void create() { write_file(\"/leak\", \"bad\"); }",
            master("void create() { throw(\"initialization failed\"); }"),
            "initialization failed",
        ),
    ] {
        let (root, mut vm, user) = setup(&format!("system-reload-fail-{label}")).await;
        let old_master = vm.global_state.object_space.master_object().unwrap();
        let old_simul = vm
            .global_state
            .object_space
            .lookup("/secure/simul_efuns")
            .unwrap();
        std::fs::write(root.join("secure/simul_efuns.c"), simul).unwrap();
        std::fs::write(root.join("secure/master.c"), replacement_master).unwrap();
        let status = run(&mut vm, &user, "both").await;
        assert_eq!(status.state, "failed", "{label}");
        assert!(status.error.contains(message), "{}", status.error);
        assert!(Arc::ptr_eq(
            &old_master,
            &vm.global_state.object_space.master_object().unwrap()
        ));
        assert!(Arc::ptr_eq(
            &old_simul,
            &vm.global_state
                .object_space
                .lookup("/secure/simul_efuns")
                .unwrap()
        ));
        assert_eq!(
            call(&vm, &user, "direct", &[]).await.unwrap(),
            LpcRef::from(11)
        );
        assert!(!root.join("leak").exists());
    }
}

#[tokio::test]
async fn a_replacement_invalidates_old_policy_and_simul_reads() {
    let (root, mut vm, user) = setup("system-reload-conflict").await;
    let mut live = start_txn(&vm.global_state.committer_tx).await.unwrap();
    let txn = TxnHandle::new(Transaction::new(live.inner.clone()));
    let mut template = TaskTemplate::from(vm.global_state.clone());
    template.txn = txn.clone();
    let ctx = template.into_task_context(user.clone());
    let old_master = ctx.master_object().unwrap();
    let old_simul = ctx.simul_efuns().unwrap();
    std::fs::write(
        root.join("secure/master.c"),
        master("int policy() { return 2; }"),
    )
    .unwrap();
    let status = run(&mut vm, &user, "both").await;
    assert_eq!(status.state, "succeeded", "{}", status.error);
    assert!(Arc::ptr_eq(&old_master, &ctx.master_object().unwrap()));
    assert!(Arc::ptr_eq(&old_simul, &ctx.simul_efuns().unwrap()));
    live.disarm();
    assert!(
        commit_changeset(
            &vm.global_state.committer_tx,
            txn.with(|t| t.take_changeset())
        )
        .await
        .unwrap()
        .is_err()
    );
    let value = apply_function_in_master(
        "policy",
        &[],
        TaskTemplate::from(vm.global_state.clone()),
        Some(5000),
    )
    .await
    .unwrap()
    .unwrap();
    assert_eq!(value, LpcRef::from(2));
}

#[tokio::test]
async fn an_existing_task_resolves_simul_and_master_at_attempt_start() {
    let (root, mut vm, user) = setup("system-reload-existing-task").await;
    let mut task: Task<MAX_CALL_STACK_SIZE> =
        Task::new(TaskTemplate::from(vm.global_state.clone()).into_task_context(user.clone()));
    let old_master = vm.global_state.object_space.master_object().unwrap();
    let mut master_task: Task<MAX_CALL_STACK_SIZE> = Task::new(
        TaskTemplate::from(vm.global_state.clone()).into_task_context(old_master.clone()),
    );
    std::fs::write(
        root.join("secure/simul_efuns.c"),
        "int version() { return 77; }",
    )
    .unwrap();
    std::fs::write(
        root.join("secure/master.c"),
        master("int policy() { return 88; }"),
    )
    .unwrap();
    assert_eq!(run(&mut vm, &user, "both").await.state, "succeeded");
    task.timed_eval(
        user.initial_program()
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
                process: old_master,
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
async fn requests_are_deferred_authorized_again_and_owned_by_the_requester() {
    let (_root, mut vm, user) = setup("system-reload-request").await;
    let failing = vm
        .initialize_process_from_code(
            "/aborting.c",
            "void create() { request_system_reload(\"master\"); throw(\"abort\"); }",
        )
        .await;
    assert!(failing.is_err());
    assert_eq!(vm.next_op(), None);
    let request = request(&mut vm, &user, "master").await;
    assert_eq!(
        vm.global_state.updates.get(request.id).unwrap().state,
        "queued"
    );
    let stranger = vm
        .initialize_process_from_code(
            "/stranger.c",
            "mapping status(int id) { return query_system_reload(id); }",
        )
        .await
        .unwrap()
        .context
        .process;
    assert!(
        call(&vm, &stranger, "status", &[request.id.into()])
            .await
            .unwrap_err()
            .to_string()
            .contains("permission denied")
    );
    vm.initialize_process_from_code("/secure/master.c", "int valid_reload() { return 0; }")
        .await
        .unwrap();
    vm.global_state.run_object_update(request.clone()).await;
    let status = vm.global_state.updates.get(request.id).unwrap();
    assert_eq!(status.state, "failed");
    assert!(status.error.contains("permission denied"));
    assert!(
        call(&vm, &user, "request", &["master".into()])
            .await
            .is_err()
    );
    assert!(
        call(&vm, &user, "request", &["unknown".into()])
            .await
            .is_err()
    );
}

#[tokio::test]
async fn retired_objects_lose_scheduled_work() {
    let (root, mut vm, user) = setup("system-reload-callout").await;
    vm.initialize_process_from_code(
        "/secure/simul_efuns.c",
        "int version() { return 1; } void later() {} void create() { call_out(later, 100); }",
    )
    .await
    .unwrap();
    assert_eq!(vm.global_state.with_call_outs(|calls| calls.len()), 1);
    std::fs::write(
        root.join("secure/simul_efuns.c"),
        "int version() { return 2; } void later() {} void create() {}",
    )
    .unwrap();
    let status = run(&mut vm, &user, "simul_efun").await;
    assert_eq!(status.state, "succeeded", "{}", status.error);
    assert_eq!(vm.global_state.with_call_outs(|calls| calls.len()), 0);
}

#[tokio::test]
async fn concurrent_requests_commit_complete_replacements() {
    let (root, mut vm, user) = setup("system-reload-concurrent").await;
    std::fs::write(
        root.join("secure/master.c"),
        master("void create() { write_file(\"/runs\", \"x\"); }"),
    )
    .unwrap();
    let first = request(&mut vm, &user, "master").await;
    let second = request(&mut vm, &user, "master").await;
    tokio::join!(
        vm.global_state.run_object_update(first.clone()),
        vm.global_state.run_object_update(second.clone())
    );
    assert_eq!(
        vm.global_state.updates.get(first.id).unwrap().state,
        "succeeded"
    );
    assert_eq!(
        vm.global_state.updates.get(second.id).unwrap().state,
        "succeeded"
    );
    assert_eq!(std::fs::read_to_string(root.join("runs")).unwrap(), "xx");
}

#[tokio::test]
async fn preparation_uses_old_simul_functions_for_authorization() {
    let (root, mut vm, user) = setup("system-reload-authority").await;
    vm.initialize_process_from_code("/secure/simul_efuns.c", "int version() { return 1; }")
        .await
        .unwrap();
    let policy = format!(
        "{PERMISSIVE_MASTER} int valid_reload() {{ return 1; }} int valid_write() {{ return version() == 1; }}"
    );
    vm.initialize_process_from_code("/secure/master.c", &policy)
        .await
        .unwrap();
    std::fs::write(
        root.join("secure/simul_efuns.c"),
        "int version() { return 2; }",
    )
    .unwrap();
    std::fs::write(
        root.join("secure/master.c"),
        format!("{policy} void create() {{ write_file(\"/authorized\", \"old policy\"); }}"),
    )
    .unwrap();
    let status = run(&mut vm, &user, "both").await;
    assert_eq!(status.state, "succeeded", "{}", status.error);
    assert_eq!(
        std::fs::read_to_string(root.join("authorized")).unwrap(),
        "old policy"
    );
}

#[tokio::test]
async fn callbacks_prepared_before_reload_resolve_new_simul_functions() {
    let (root, mut vm, user) = setup("system-reload-prepared").await;
    let function =
        match crate::interpreter::CommittedReader::committed_global(&vm.global_state, &user, 0) {
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
        "int version() { write_file(\"/callback\", \"new\"); return 2; }",
    )
    .unwrap();
    assert_eq!(run(&mut vm, &user, "simul_efun").await.state, "succeeded");
    prepared.execute(5000).await.unwrap();
    assert_eq!(
        std::fs::read_to_string(root.join("callback")).unwrap(),
        "new"
    );
}

#[tokio::test]
async fn delayed_scheduling_cannot_restore_a_retired_owners_callout() {
    let (_root, mut vm, user) = setup("system-reload-delayed-callout").await;
    let old = vm
        .global_state
        .object_space
        .lookup("/secure/simul_efuns")
        .unwrap();
    let deferred = crate::interpreter::stm::CallOutSchedule {
        id: 100000,
        process: Arc::downgrade(&old),
        func_ref: 0.into(),
        delay: chrono::Duration::seconds(100),
        repeat: None,
    };
    assert_eq!(run(&mut vm, &user, "simul_efun").await.state, "succeeded");
    Effect::ScheduleCallOut(deferred)
        .flush(&vm.global_state)
        .await;
    assert_eq!(vm.global_state.with_call_outs(|calls| calls.len()), 0);
}

#[tokio::test]
async fn recursive_reload_and_structural_changes_fail_without_publication() {
    for (name, initializer, message) in [
        (
            "recursive",
            "request_system_reload(\"master\");",
            "during preparation",
        ),
        ("commands", "enable_commands();", "detached daemon"),
        (
            "self-destruct",
            "destruct(this_object());",
            "detached daemon",
        ),
    ] {
        let (root, mut vm, user) = setup(&format!("system-reload-{name}")).await;
        let old = vm.global_state.object_space.master_object().unwrap();
        std::fs::write(
            root.join("secure/master.c"),
            master(&format!("void create() {{ {initializer} }}")),
        )
        .unwrap();
        let status = run(&mut vm, &user, "master").await;
        assert_eq!(status.state, "failed", "{}", status.error);
        assert!(status.error.contains(message), "{}", status.error);
        assert!(Arc::ptr_eq(
            &old,
            &vm.global_state.object_space.master_object().unwrap()
        ));
    }
}

#[tokio::test]
async fn a_missing_reload_hook_refuses_and_an_unknown_status_is_zero() {
    let (_root, mut vm, user) = setup("system-reload-missing-hook").await;
    assert_eq!(
        call(&vm, &user, "status", &[999.into()]).await.unwrap(),
        LpcRef::from(0)
    );
    vm.initialize_process_from_code("/secure/master.c", "")
        .await
        .unwrap();
    assert!(
        call(&vm, &user, "request", &["master".into()])
            .await
            .unwrap_err()
            .to_string()
            .contains("permission denied")
    );
    assert_eq!(vm.next_op(), None);
}

#[tokio::test]
async fn rejected_preparation_reauthorizes_and_delivers_effects_once() {
    let (root, mut vm, user) = setup("system-reload-retry").await;
    std::fs::write(
        root.join("secure/master.c"),
        master("void create() { write_file(\"/attempts\", \"x\"); }"),
    )
    .unwrap();
    let first = request(&mut vm, &user, "master").await;
    let second = request(&mut vm, &user, "master").await;
    let gs = &vm.global_state;
    let mut body = UpdateBody {
        gs,
        request: &first,
        txn: None,
        updated: 0,
    };
    let mut original = body.begin_attempt(&gs.committer_tx).await.unwrap().unwrap();
    gs.run_object_update(second.clone()).await;
    assert_eq!(gs.updates.get(second.id).unwrap().state, "succeeded");
    original.disarm();
    let (commit, discarded) = body.commit_phase(&gs.committer_tx, original).await.unwrap();
    assert!(commit.is_err());
    drop(discarded);
    assert_eq!(std::fs::read_to_string(root.join("attempts")).unwrap(), "x");
    let mut retry = body.begin_attempt(&gs.committer_tx).await.unwrap().unwrap();
    retry.disarm();
    let (commit, effects) = body.commit_phase(&gs.committer_tx, retry).await.unwrap();
    commit.unwrap();
    body.deliver(effects).await.unwrap();
    assert_eq!(
        std::fs::read_to_string(root.join("attempts")).unwrap(),
        "xx"
    );
}

#[tokio::test]
async fn deferred_authorization_preserves_caller_program_and_command_giver() {
    let (_root, mut vm, user) = setup("system-reload-provenance").await;
    vm.initialize_process_from_code(
        "/secure/master.c",
        r#"
        int valid_reload(string target, object caller, string program) {
            return target == "master" && caller == find_object("/admin")
                && program == "/admin.c" && this_player() == caller;
        }
    "#,
    )
    .await
    .unwrap();
    let template = TaskTemplate::from(vm.global_state.clone());
    template.set_this_player(Some(user.clone()));
    apply_function_by_name("request", &["master".into()], user, template, Some(5000))
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
async fn missing_configuration_and_missing_resident_are_refused() {
    let root = lib_holding(
        "system-reload-missing-config",
        &[("secure/master.c", &master(""))],
    );
    let mut vm = Vm::new(crate::test_support::temp_lib_config(&root));
    vm.bootstrap().await.unwrap();
    let user = vm
        .initialize_process_from_code(
            "/admin.c",
            "int request(string target) { return request_system_reload(target); }",
        )
        .await
        .unwrap()
        .context
        .process;
    let err = call(&vm, &user, "request", &["simul_efun".into()])
        .await
        .unwrap_err();
    assert!(err.to_string().contains("no simul-efun source"));
    assert_eq!(vm.next_op(), None);

    let (_root, mut vm, user) = setup("system-reload-missing-resident").await;
    vm.initialize_process_from_code(
        "/destroyer.c",
        "void create() { destruct(find_object(\"/secure/simul_efuns\")); }",
    )
    .await
    .unwrap();
    let status = run(&mut vm, &user, "simul_efun").await;
    assert_eq!(status.state, "failed");
    assert!(status.error.contains("no simul-efun object"));
}

#[tokio::test]
async fn garbage_collection_cannot_allow_a_retired_initializer_to_republish() {
    let (_root, mut vm, user) = setup("system-reload-retired-init").await;
    let old = vm.global_state.object_space.master_object().unwrap();
    assert_eq!(run(&mut vm, &user, "master").await.state, "succeeded");
    let current = vm.global_state.object_space.master_object().unwrap();
    vm.global_state.gc().await.unwrap().unwrap();
    let context = TaskTemplate::from(vm.global_state.clone()).into_task_context(old);
    assert!(
        Task::<MAX_CALL_STACK_SIZE>::initialize_process(context)
            .await
            .is_err()
    );
    assert!(Arc::ptr_eq(
        &current,
        &vm.global_state.object_space.master_object().unwrap()
    ));
}
