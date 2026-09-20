use std::sync::Arc;

use crate::{
    interpreter::{
        CommittedReader,
        lpc_ref::LpcRef,
        process::Process,
        task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
        vm::{Vm, vm_op::VmOp},
    },
    test_support::{PERMISSIVE_MASTER, TempLib, lib_holding, temp_lib_config},
};

async fn setup(
    name: &str,
    source: &str,
    files: &[(&str, &str)],
) -> (TempLib, Vm, Arc<Process>, Arc<Process>) {
    let master = format!(
        r#"{PERMISSIVE_MASTER}
        string failure;
        int valid_recompile() {{ return 1; }}
        void error_handler(mapping error) {{ failure = error["error"]; }}
    "#
    );
    let mut sources = vec![("secure/master.c", master.as_str()), ("target.c", source)];
    sources.extend_from_slice(files);
    let root = lib_holding(name, &sources);
    let vm = Vm::new(temp_lib_config(&root));
    vm.initialize_process_from_code("/secure/master.c", &master)
        .await
        .unwrap();
    let admin = vm
        .initialize_process_from_code(
            "/admin.c",
            r#"
        object target = load_object("/target");
        int request() { return request_object_recompile(target); }
    "#,
        )
        .await
        .unwrap()
        .context
        .process;
    let target = vm.global_state.object_space.lookup("/target").unwrap();
    (root, vm, admin, target)
}

async fn call(vm: &Vm, target: &Arc<Process>, name: &str) -> LpcRef {
    apply_function_by_name(
        name,
        &[],
        target.clone(),
        TaskTemplate::from(vm.global_state.clone()),
        Some(5000),
    )
    .await
    .unwrap()
    .unwrap()
}

async fn upgrade(root: &TempLib, vm: &mut Vm, admin: &Arc<Process>, source: &str) {
    std::fs::write(root.join("target.c"), source).unwrap();
    call(vm, admin, "request").await;
    let Some(VmOp::ObjectUpdate(request)) = vm.next_op() else {
        panic!("upgrade queued");
    };
    vm.global_state.run_object_update(request.clone()).await;
    let status = vm.global_state.updates.get(request.id).unwrap();
    assert_eq!(status.state, "succeeded", "{}", status.error);
}

fn timer(vm: &Vm) -> u64 {
    vm.global_state.with_call_outs(|calls| {
        assert_eq!(calls.len(), 1);
        calls.queue().iter().next().unwrap().1.id
    })
}

#[tokio::test]
async fn repeating_named_callouts_use_each_updated_program_and_keep_their_schedule() {
    let original = r#"
        int ticks;
        void tick() { ticks += 1; }
        void create() { call_out(tick, 3600, 3600); }
        int result() { return ticks; }
    "#;
    let (root, mut vm, admin, target) = setup("recompile-repeat", original, &[]).await;
    let id = timer(&vm);
    let updated = format!(
        "int padding = 777; {}",
        original.replace("ticks += 1;", "ticks += 10;")
    );
    upgrade(&root, &mut vm, &admin, &updated).await;
    vm.global_state.gc().await.unwrap().unwrap();
    vm.global_state.prioritize_call_out(id).await.await.unwrap();
    assert_eq!(call(&vm, &target, "result").await, LpcRef::from(10));
    assert_eq!(timer(&vm), id);
    upgrade(
        &root,
        &mut vm,
        &admin,
        &updated.replace("ticks += 10;", "ticks += 100;"),
    )
    .await;
    vm.global_state.prioritize_call_out(id).await.await.unwrap();
    assert_eq!(call(&vm, &target, "result").await, LpcRef::from(110));
    assert_eq!(timer(&vm), id);
}

#[tokio::test]
async fn anonymous_callouts_keep_captures_old_code_and_retired_globals_through_gc() {
    let original = r#"
        int changed = 5;
        int removed = 7;
        int shared = 10;
        int observed;
        int helper() { return 3; }
        void create() {
            int local = 2;
            call_out((: observed = ++local + ++shared + ++changed + ++removed + helper() :), 3600, 3600);
        }
        int result() { return observed; }
    "#;
    let updated = r#"
        int padding = 999;
        int observed;
        int shared = 900;
        string changed = "new";
        int helper() { return 100; }
        int result() { return observed; }
        int intact() { return shared == 12 && changed == "new"; }
    "#;
    let (root, mut vm, admin, target) = setup("recompile-closure-globals", original, &[]).await;
    let id = timer(&vm);
    upgrade(&root, &mut vm, &admin, updated).await;
    for expected in [31, 35] {
        vm.global_state.gc().await.unwrap().unwrap();
        vm.global_state.prioritize_call_out(id).await.await.unwrap();
        assert_eq!(call(&vm, &target, "result").await, LpcRef::from(expected));
        assert_eq!(timer(&vm), id);
    }
    assert_eq!(call(&vm, &target, "intact").await, LpcRef::from(1));
}

#[tokio::test]
async fn retained_closures_distinguish_lexical_inherited_calls_from_external_self_calls() {
    let base = "int base = 5; int value() { return base; }";
    let original = r#"
        inherit "/base" parent;
        int number = 7;
        function saved;
        void create() { saved = (: helper() + this_object()->helper() + call_inherited("parent::value") + inherited_through_pointer() :); }
        int helper() { return number; }
        int inherited_through_pointer() { function invoke = &call_inherited("parent::value"); return invoke(); }
        int result() { return saved(); }
    "#;
    let updated = r#"
        inherit "/padding";
        inherit "/base" parent;
        function saved;
        int number = 900;
        int helper() { return number * 10; }
        int result() { return saved(); }
    "#;
    let (root, mut vm, admin, target) = setup(
        "recompile-closure-dispatch",
        original,
        &[("base.c", base), ("padding.c", "int padding = 888;")],
    )
    .await;
    std::fs::write(
        root.join("base.c"),
        base.replace("return base;", "return base * 100;"),
    )
    .unwrap();
    upgrade(&root, &mut vm, &admin, updated).await;
    vm.global_state.gc().await.unwrap().unwrap();
    assert_eq!(call(&vm, &target, "result").await, LpcRef::from(87));
}

#[tokio::test]
async fn a_closure_created_by_retained_code_keeps_that_codes_global_layout() {
    let original = r#"
        int number = 7;
        function maker;
        void create() { maker = (: (: ++number :) :); }
        int result() { function inner = maker(); return inner(); }
    "#;
    let updated = r#"
        string padding = "new";
        function maker;
        int number = 900;
        int result() { function inner = maker(); return inner(); }
    "#;
    let (root, mut vm, admin, target) = setup("recompile-nested-closure", original, &[]).await;
    upgrade(&root, &mut vm, &admin, updated).await;
    vm.global_state.gc().await.unwrap().unwrap();
    assert_eq!(call(&vm, &target, "result").await, LpcRef::from(8));
    assert_eq!(call(&vm, &target, "result").await, LpcRef::from(9));
}

#[tokio::test]
async fn private_inherited_named_pointers_keep_their_declaration_when_rebound() {
    let base = r#"
        int base = 5;
        private int tick() { return base; }
        function inherited_pointer() { return &tick(); }
    "#;
    let original = r#"
        inherit "/base";
        int own = 7;
        function inherited;
        function local;
        private int own_tick() { return own; }
        void create() { inherited = inherited_pointer(); local = &own_tick(); }
        int result() { return inherited() + local(); }
    "#;
    let (root, mut vm, admin, target) =
        setup("recompile-private-binding", original, &[("base.c", base)]).await;
    assert_eq!(call(&vm, &target, "result").await, LpcRef::from(12));
    std::fs::write(
        root.join("base.c"),
        base.replace("return base;", "return base * 10;"),
    )
    .unwrap();
    upgrade(
        &root,
        &mut vm,
        &admin,
        &format!(
            "{} private int tick() {{ return 10000; }}",
            original.replace("return own;", "return own * 100;")
        ),
    )
    .await;
    assert_eq!(call(&vm, &target, "result").await, LpcRef::from(750));
}

#[tokio::test]
async fn incompatible_or_removed_named_callbacks_fail_explicitly() {
    let original = r#"
        int hits;
        void tick(int n) { hits += n; }
        void create() { call_out(&tick(2), 3600, 3600); }
        int result() { return hits; }
    "#;
    for (label, replacement) in [
        ("removed", "void other(int n) { hits += n; }"),
        ("return", "int tick(int n) { return hits += n; }"),
        ("argument", "void tick(string n) { hits++; }"),
        ("visibility", "private void tick(int n) { hits += n; }"),
        ("default", "void tick(int n = 9) { hits += n; }"),
        ("reference", "void tick(int ref n) { hits += n; }"),
    ] {
        let (root, mut vm, admin, target) =
            setup(&format!("recompile-callback-{label}"), original, &[]).await;
        let id = timer(&vm);
        let updated = format!("int hits; {replacement} int result() {{ return hits; }}");
        upgrade(&root, &mut vm, &admin, &updated).await;
        vm.global_state.prioritize_call_out(id).await.await.unwrap();
        let master = vm.global_state.object_space.master_object().unwrap();
        let error = crate::test_support::committed_string(&vm, &master, 0);
        assert!(
            error.contains("missing or incompatible"),
            "{label}: {error}"
        );
        assert_eq!(call(&vm, &target, "result").await, LpcRef::from(0));
        assert_eq!(vm.global_state.with_call_outs(|calls| calls.len()), 0);
    }
}

#[tokio::test]
async fn composed_callbacks_survive_their_owners_upgrade() {
    let original = r#"
        int total;
        void set(int n) { total = n; }
        void create() { int captured = 5; call_out(compose(&set(), (: captured :)), 3600); }
        int result() { return total; }
    "#;
    let (root, mut vm, admin, target) = setup("recompile-composed-callback", original, &[]).await;
    let id = timer(&vm);
    upgrade(
        &root,
        &mut vm,
        &admin,
        &original.replace("total = n;", "total = n * 2;"),
    )
    .await;
    vm.global_state.gc().await.unwrap().unwrap();
    vm.global_state.prioritize_call_out(id).await.await.unwrap();
    assert_eq!(call(&vm, &target, "result").await, LpcRef::from(10));
}

#[tokio::test]
async fn a_prepared_anonymous_callback_retains_its_code_and_captures_across_upgrade() {
    let original = r#"
        int total;
        function callback;
        void create() { int captured = 5; callback = (: total += ++captured :); }
        int result() { return total; }
    "#;
    let (root, mut vm, admin, target) = setup("recompile-prepared-closure", original, &[]).await;
    let LpcRef::Function(callback) = vm.global_state.committed_global(&target, 1) else {
        panic!("closure");
    };
    let prepared = vm
        .global_state
        .prepare_function_ptr(&callback, &[], None)
        .await
        .unwrap()
        .unwrap();
    upgrade(&root, &mut vm, &admin, &format!("int padding; {original}")).await;
    vm.global_state.gc().await.unwrap().unwrap();
    prepared.execute(5000).await.unwrap();
    assert_eq!(call(&vm, &target, "result").await, LpcRef::from(6));
}

#[tokio::test]
async fn cancelling_the_last_retaining_callout_releases_retired_global_cells() {
    use crate::interpreter::stm::start_txn;
    let original = r#"
        int ticket;
        int removed = 77;
        int shared;
        void create() { ticket = call_out((: shared = ++removed :), 3600, 3600); }
    "#;
    let updated = r#"
        int shared;
        int ticket;
        void stop() { remove_call_out(ticket); }
    "#;
    let (root, mut vm, admin, target) = setup("recompile-closure-reclamation", original, &[]).await;
    let removed = target.initial_image().var_id(1);
    upgrade(&root, &mut vm, &admin, updated).await;
    vm.global_state.gc().await.unwrap().unwrap();
    let live = start_txn(&vm.global_state.committer_tx).await.unwrap();
    assert!(live.inner.read(removed).is_some());
    drop(live);
    call(&vm, &target, "stop").await;
    vm.global_state.gc().await.unwrap().unwrap();
    let live = start_txn(&vm.global_state.committer_tx).await.unwrap();
    assert!(live.inner.read(removed).is_none());
}

#[tokio::test]
async fn registered_actions_use_updated_handlers_without_reregistering() {
    let original = r#"
        int hits;
        void create() {
            set_this_player(this_object());
            enable_commands();
            add_action("tick", "tick");
        }
        int tick(string text) { hits += 1; return 1; }
        int invoke() { return command("tick text"); }
        int result() { return hits; }
    "#;
    let (root, mut vm, admin, target) = setup("recompile-action-callback", original, &[]).await;
    upgrade(
        &root,
        &mut vm,
        &admin,
        &original.replace("hits += 1;", "hits += 10;"),
    )
    .await;
    assert_eq!(call(&vm, &target, "invoke").await, LpcRef::from(1));
    assert_eq!(call(&vm, &target, "result").await, LpcRef::from(10));
}
