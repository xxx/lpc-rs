use super::*;
use crate::interpreter::{
    stm::{AttemptBody, Transaction, TxnHandle},
    task::task_template::TaskTemplate,
    vm::object_update::{UpdateBody, UpdateRequest, UpdateStatus},
};
use crate::{
    interpreter::{
        CommittedReader,
        stm::{commit_changeset, start_txn},
        task::apply_function::apply_function_by_name,
        vm::{Vm, vm_op::VmOp},
    },
    test_support::{PERMISSIVE_MASTER, TempLib, lib_holding, temp_lib_config},
};

const ORIGINAL: &str = r#"
    int health = 10;
    int creates;
    mixed changed = 11;
    int removed = 99;
    void create() { creates++; }
    void set(int value) { health = value; }
    void relocate(object destination) { move_object(destination); }
    int value() { return health; }
    int created() { return creates; }
    function closure() { return (: health :); }
"#;

const UPDATED: &str = r#"
    int added = 7;
    string changed = "new";
    int creates = 999;
    int health = 900;
    void create() { creates += 1000; }
    void set(int value) { health = value; }
    int value() { return health * 10; }
    int created() { return creates; }
    int defaults() { return added == 7 && changed == "new"; }
    function composed() { return compose(&abs(), &abs()); }
    function closure() { return (: health :); }
"#;

const ADMIN: &str = r#"
    object prototype;
    object first;
    object second;
    function local;
    function dynamic;
    function anonymous;
    void create() {
        prototype = load_object("/target");
        first = clone_object("/target");
        second = clone_object("/target");
        first->set(21);
        second->set(33);
        local = &(first)->value();
        dynamic = papplyv(&->value(), ({ first }));
        anonymous = first->closure();
    }
    int request() { return request_object_recompile(prototype); }
    mapping status(int id) { return query_object_recompile(id); }
    int sum() { return prototype->value() + first->value() + second->value(); }
    int check() {
        return prototype == find_object("/target")
            && first == find_object(file_name(first))
            && sizeof(object_clones(prototype)) == 2
            && sizeof(object_clones(first)) == 2
            && first->created() == 1 && second->created() == 1
            && prototype->defaults() && first->defaults() && second->defaults();
    }
    int pointers_fail() { return !!catch(local()) || !!catch(anonymous()); }
    int local_value() { return local(); }
    int closure_value() { return anonymous(); }
    int dynamic_value() { return dynamic(); }
    int new_clone() { return clone_object("/target")->value(); }
    void abort() { request(); throw("abort request"); }
    void destroy_target() { destruct(prototype); }
    void destroy_clone() { destruct(second); }
    void arm_callbacks() {
        call_out(papplyv(&(first)->set(), ({ 55 })), 3600);
        call_out(papplyv(&->set(), ({ first, 77 })), 3600);
    }
    void arrange() { first->relocate(this_object()); second->relocate(first); }
    int position_intact() {
        return environment(first) == this_object() && environment(second) == first
            && all_inventory(this_object())[0] == first && all_inventory(first)[0] == second;
    }
    int invalid_targets() {
        return !!catch(request_object_recompile(0))
            && !!catch(request_object_recompile(first))
            && !!catch(request_object_recompile("invalid"));
    }
"#;

async fn setup(name: &str) -> (TempLib, Vm, Arc<Process>) {
    let master = format!(
        "{PERMISSIVE_MASTER}\nint valid_recompile(object p, object c, string s) {{ return 1; }}\nint valid_write() {{ return 1; }}\nint valid_variable_info() {{ return 1; }}"
    );
    let root = lib_holding(
        name,
        &[("target.c", ORIGINAL), ("secure/master.c", &master)],
    );
    let vm = Vm::new(temp_lib_config(&root));
    vm.initialize_process_from_code("/secure/master.c", &master)
        .await
        .unwrap();
    let admin = vm
        .initialize_process_from_code("/admin.c", ADMIN)
        .await
        .unwrap()
        .context
        .process;
    (root, vm, admin)
}

async fn call(vm: &Vm, process: &Arc<Process>, name: &str, args: &[LpcRef]) -> Result<LpcRef> {
    apply_function_by_name(
        name,
        args,
        process.clone(),
        TaskTemplate::from(vm.global_state.clone()),
        Some(5000),
    )
    .await
    .expect("function exists")
}

async fn request(vm: &mut Vm, admin: &Arc<Process>) -> Arc<UpdateRequest> {
    let id = call(vm, admin, "request", &[]).await.unwrap();
    let Some(VmOp::ObjectUpdate(request)) = vm.next_op() else {
        panic!("recompilation queued");
    };
    assert_eq!(id, LpcRef::from(request.id));
    request
}

async fn run(vm: &mut Vm, admin: &Arc<Process>) -> UpdateStatus {
    let request = request(vm, admin).await;
    vm.global_state.run_object_update(request.clone()).await;
    vm.global_state.updates.get(request.id).unwrap()
}

#[tokio::test]
async fn upgrades_identity_and_independent_state_with_new_defaults() {
    let (root, mut vm, admin) = setup("recompile-state").await;
    let prototype = vm.global_state.object_space.lookup("/target").unwrap();
    let first = vm.global_state.object_space.lookup("/target#0").unwrap();
    let created = first.created;
    call(&vm, &admin, "arrange", &[]).await.unwrap();
    assert_eq!(
        call(&vm, &admin, "sum", &[]).await.unwrap(),
        LpcRef::from(64)
    );
    std::fs::write(root.join("target.c"), UPDATED).unwrap();
    let status = run(&mut vm, &admin).await;
    assert_eq!(status.state, "succeeded", "{}", status.error);
    assert_eq!(status.updated, 3);
    assert!(Arc::ptr_eq(
        &prototype,
        &vm.global_state.object_space.lookup("/target").unwrap()
    ));
    assert!(Arc::ptr_eq(
        &first,
        &vm.global_state.object_space.lookup("/target#0").unwrap()
    ));
    assert_eq!(first.created, created);
    assert_eq!(
        call(&vm, &admin, "position_intact", &[]).await.unwrap(),
        LpcRef::from(1)
    );
    assert_eq!(
        call(&vm, &admin, "sum", &[]).await.unwrap(),
        LpcRef::from(640)
    );
    assert_eq!(
        call(&vm, &admin, "check", &[]).await.unwrap(),
        LpcRef::from(1)
    );
    assert_eq!(
        call(&vm, &admin, "pointers_fail", &[]).await.unwrap(),
        LpcRef::from(0)
    );
    assert_eq!(
        call(&vm, &admin, "local_value", &[]).await.unwrap(),
        LpcRef::from(210)
    );
    assert_eq!(
        call(&vm, &admin, "closure_value", &[]).await.unwrap(),
        LpcRef::from(21)
    );
    assert_eq!(
        call(&vm, &admin, "dynamic_value", &[]).await.unwrap(),
        LpcRef::from(210)
    );
    assert_eq!(
        call(&vm, &admin, "new_clone", &[]).await.unwrap(),
        LpcRef::from(9000)
    );
    assert_eq!(vm.global_state.global_slot_count(&first), 4);
    assert_eq!(
        vm.global_state.committed_global(&first, 3),
        LpcRef::from(21)
    );
    vm.global_state.gc().await.unwrap().unwrap();
    assert_eq!(
        call(&vm, &admin, "sum", &[]).await.unwrap(),
        LpcRef::from(640)
    );
    let repeated = run(&mut vm, &admin).await;
    assert_eq!(repeated.state, "succeeded", "{}", repeated.error);
    assert_eq!(repeated.updated, 4);
}

#[tokio::test]
async fn compilation_and_initializer_failures_leave_the_whole_group_unchanged() {
    let (root, mut vm, admin) = setup("recompile-failure").await;
    for source in [
        "int broken = ;",
        r#"
        int health = side_effect();
        int side_effect() { write_file("/leak", "bad"); throw("initializer failed"); }
        int value() { return health * 100; }
    "#,
    ] {
        std::fs::write(root.join("target.c"), source).unwrap();
        let status = run(&mut vm, &admin).await;
        assert_eq!(status.state, "failed");
        assert_eq!(status.updated, 0);
        assert!(!status.error.is_empty());
        assert_eq!(
            call(&vm, &admin, "sum", &[]).await.unwrap(),
            LpcRef::from(64)
        );
        assert_eq!(
            call(&vm, &admin, "pointers_fail", &[]).await.unwrap(),
            LpcRef::from(0)
        );
        assert!(!root.join("leak").exists());
    }
}

#[tokio::test]
async fn permission_is_required_and_rechecked_and_aborted_requests_are_not_queued() {
    let (_root, mut vm, admin) = setup("recompile-permission").await;
    assert!(call(&vm, &admin, "abort", &[]).await.is_err());
    assert!(vm.next_op().is_none());
    let queued = request(&mut vm, &admin).await;
    vm.initialize_process_from_code("/secure/master.c", PERMISSIVE_MASTER)
        .await
        .unwrap();
    vm.global_state.run_object_update(queued.clone()).await;
    let status = vm.global_state.updates.get(queued.id).unwrap();
    assert_eq!(status.state, "failed");
    assert!(status.error.contains("permission denied"));
    assert!(
        call(&vm, &admin, "request", &[])
            .await
            .unwrap_err()
            .to_string()
            .contains("permission denied")
    );
}

#[tokio::test]
async fn retired_target_cannot_upgrade_a_replacement_under_its_name() {
    let (_root, mut vm, admin) = setup("recompile-retired").await;
    let queued = request(&mut vm, &admin).await;
    call(&vm, &admin, "destroy_target", &[]).await.unwrap();
    vm.initialize_process_from_code("/target.c", ORIGINAL)
        .await
        .unwrap();
    vm.global_state.run_object_update(queued.clone()).await;
    assert_eq!(
        vm.global_state.updates.get(queued.id).unwrap().state,
        "failed"
    );
}

#[tokio::test]
async fn an_old_attempt_keeps_its_layout_but_conflicts_after_publication() {
    let (root, mut vm, admin) = setup("recompile-conflict").await;
    let target = vm.global_state.object_space.lookup("/target#0").unwrap();
    let live = start_txn(&vm.global_state.committer_tx).await.unwrap();
    let txn = TxnHandle::new(Transaction::new(live.inner.clone()));
    let image = target.image(&txn);
    let function = image.program.lookup_function("value").unwrap().clone();
    let mut context = TaskTemplate::from(vm.global_state.clone()).into_task_context(target.clone());
    context.txn = txn.clone();
    std::fs::write(root.join("target.c"), UPDATED).unwrap();
    assert_eq!(run(&mut vm, &admin).await.state, "succeeded");
    let mut task = Task::<MAX_CALL_STACK_SIZE>::new(context);
    task.timed_eval(function, &[], 5000).await.unwrap();
    assert_eq!(task.result().unwrap(), LpcRef::from(21));
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
        call(&vm, &target, "value", &[]).await.unwrap(),
        LpcRef::from(210)
    );
}

#[tokio::test]
async fn prepared_named_and_dynamic_callbacks_resolve_the_updated_program() {
    let (root, mut vm, admin) = setup("recompile-callback").await;
    let LpcRef::Function(local) = vm.global_state.committed_global(&admin, 3) else {
        panic!("local pointer");
    };
    let LpcRef::Function(dynamic) = vm.global_state.committed_global(&admin, 4) else {
        panic!("dynamic pointer");
    };
    let local = vm
        .global_state
        .prepare_function_ptr(&local, &[], None)
        .await
        .unwrap()
        .unwrap();
    let dynamic = vm
        .global_state
        .prepare_function_ptr(&dynamic, &[], None)
        .await
        .unwrap()
        .unwrap();
    std::fs::write(
        root.join("target.c"),
        UPDATED.replace("return health * 10;", "health++; return health * 10;"),
    )
    .unwrap();
    assert_eq!(run(&mut vm, &admin).await.state, "succeeded");
    let first = vm.global_state.object_space.lookup("/target#0").unwrap();
    local.execute(5000).await.unwrap();
    assert_eq!(
        vm.global_state.committed_global(&first, 3),
        LpcRef::from(22)
    );
    dynamic.execute(5000).await.unwrap();
    assert_eq!(
        vm.global_state.committed_global(&first, 3),
        LpcRef::from(23)
    );
}

#[tokio::test]
async fn authorized_status_queries_and_duplicate_delivery_do_not_upgrade_again() {
    let (_root, mut vm, admin) = setup("recompile-status").await;
    let queued = request(&mut vm, &admin).await;
    let other = vm
        .initialize_process_from_code(
            "/other.c",
            "string status(int id) { return query_object_recompile(id)[\"state\"]; }",
        )
        .await
        .unwrap()
        .context
        .process;
    assert_eq!(
        call(&vm, &other, "status", &[queued.id.into()])
            .await
            .unwrap(),
        LpcRef::from("queued")
    );
    assert_eq!(
        call(&vm, &admin, "status", &[(-1).into()]).await.unwrap(),
        NULL
    );
    vm.global_state.run_object_update(queued.clone()).await;
    let target = vm.global_state.object_space.lookup("/target").unwrap();
    let live = start_txn(&vm.global_state.committer_tx).await.unwrap();
    let txn = TxnHandle::new(Transaction::new(live.inner.clone()));
    let generation = target.image(&txn).generation;
    drop(live);
    vm.global_state.run_object_update(queued.clone()).await;
    let live = start_txn(&vm.global_state.committer_tx).await.unwrap();
    let txn = TxnHandle::new(Transaction::new(live.inner.clone()));
    assert_eq!(target.image(&txn).generation, generation);
    assert_eq!(vm.global_state.updates.get(queued.id).unwrap().updated, 3);
    assert_eq!(
        call(&vm, &other, "status", &[queued.id.into()])
            .await
            .unwrap(),
        LpcRef::from("succeeded")
    );
}

#[tokio::test]
async fn status_requires_permission_except_for_the_requester_and_unknown_ids() {
    for (policy, error) in [
        ("int valid_recompile() { return 0; }", "permission denied"),
        ("", "permission denied"),
        (
            "int valid_recompile() { throw(\"query refused\"); }",
            "query refused",
        ),
    ] {
        let (_root, mut vm, admin) = setup("recompile-status-permission").await;
        let queued = request(&mut vm, &admin).await;
        let other = vm
            .initialize_process_from_code(
                "/other.c",
                "mapping status(int id) { return query_object_recompile(id); }",
            )
            .await
            .unwrap()
            .context
            .process;
        vm.initialize_process_from_code(
            "/secure/master.c",
            format!("{PERMISSIVE_MASTER}\n{policy}"),
        )
        .await
        .unwrap();
        let denied = call(&vm, &other, "status", &[queued.id.into()])
            .await
            .unwrap_err();
        assert!(denied.to_string().contains(error), "{denied}");
        assert!(matches!(
            call(&vm, &admin, "status", &[queued.id.into()])
                .await
                .unwrap(),
            LpcRef::Mapping(_)
        ));
        assert_eq!(
            call(&vm, &other, "status", &[(-1).into()]).await.unwrap(),
            NULL
        );
    }
}

#[tokio::test]
async fn failed_status_can_be_authorized_after_the_requester_and_target_are_gone() {
    let (_root, mut vm, admin) = setup("recompile-status-retired").await;
    let queued = request(&mut vm, &admin).await;
    call(&vm, &admin, "destroy_target", &[]).await.unwrap();
    vm.initialize_process_from_code("/target.c", ORIGINAL)
        .await
        .unwrap();
    vm.global_state.run_object_update(queued.clone()).await;
    let reader = vm
        .initialize_process_from_code(
            "/reader.c",
            r#"
                void retire() { destruct(find_object("/admin")); }
                int inspect(int id) {
                    mapping status = query_object_recompile(id);
                    return status["state"] == "failed" && !status["target"]
                        && sizeof(status["error"]) > 0;
                }
            "#,
        )
        .await
        .unwrap()
        .context
        .process;
    call(&vm, &reader, "retire", &[]).await.unwrap();
    drop(admin);
    vm.initialize_process_from_code(
        "/secure/master.c",
        format!(
            r#"{PERMISSIVE_MASTER}
            int valid_recompile(object prototype, object caller, string program) {{
                return prototype == 0 && caller == find_object("/reader")
                    && program == "/reader.c";
            }}"#
        ),
    )
    .await
    .unwrap();
    assert_eq!(
        call(&vm, &reader, "inspect", &[queued.id.into()])
            .await
            .unwrap(),
        LpcRef::from(1)
    );
}

#[tokio::test]
async fn concurrent_global_writes_cloning_and_destruction_reject_staged_migration() {
    for action in ["write", "clone", "destruct"] {
        let (root, mut vm, admin) = setup(&format!("recompile-race-{action}")).await;
        std::fs::write(root.join("target.c"), UPDATED).unwrap();
        let queued = request(&mut vm, &admin).await;
        let mut body = UpdateBody {
            gs: &vm.global_state,
            request: &queued,
            txn: None,
            updated: 0,
        };
        let live = body
            .begin_attempt(&vm.global_state.committer_tx)
            .await
            .unwrap()
            .unwrap();
        match action {
            "write" => {
                let target = vm.global_state.object_space.lookup("/target#0").unwrap();
                call(&vm, &target, "set", &[77.into()]).await.unwrap();
            }
            "clone" => {
                call(&vm, &admin, "new_clone", &[]).await.unwrap();
            }
            _ => {
                call(&vm, &admin, "destroy_clone", &[]).await.unwrap();
            }
        }
        let (committed, _) = body
            .commit_phase(&vm.global_state.committer_tx, live)
            .await
            .unwrap();
        assert!(committed.is_err(), "{action} must conflict");
        vm.global_state.run_object_update(queued.clone()).await;
        let status = vm.global_state.updates.get(queued.id).unwrap();
        assert_eq!(status.state, "succeeded", "{}", status.error);
        match action {
            "write" => {
                let target = vm.global_state.object_space.lookup("/target#0").unwrap();
                assert_eq!(
                    call(&vm, &target, "value", &[]).await.unwrap(),
                    LpcRef::from(770)
                );
            }
            "clone" => assert_eq!(status.updated, 4),
            _ => assert_eq!(status.updated, 2),
        }
    }
}

#[tokio::test]
async fn inherited_private_declarations_and_shared_values_keep_their_identity() {
    let master = format!("{PERMISSIVE_MASTER} int valid_recompile() {{ return 1; }}");
    let parent = "private int hidden = 3; int parent_value() { return hidden; }";
    let original = r#"
        inherit "/parent";
        private int hidden = 5;
        int *shared = ({ 17 });
        int own_value() { return hidden; }
        int *array() { return shared; }
    "#;
    let root = lib_holding(
        "recompile-inherited",
        &[("parent.c", parent), ("target.c", original)],
    );
    let mut vm = Vm::new(temp_lib_config(&root));
    vm.initialize_process_from_code("/secure/master.c", &master)
        .await
        .unwrap();
    let admin = vm
        .initialize_process_from_code(
            "/admin.c",
            r#"
        object prototype = load_object("/target");
        object instance = clone_object("/target");
        int *held = instance->array();
        int request() { return request_object_recompile(prototype); }
        int check() {
            held[0] = 29;
            return instance->array() == held && instance->array()[0] == 29
                && instance->parent_value() == 3 && instance->own_value() == 5;
        }
    "#,
        )
        .await
        .unwrap()
        .context
        .process;
    std::fs::write(
        root.join("parent.c"),
        "int extra = 88; private int hidden = 100; int parent_value() { return hidden; }",
    )
    .unwrap();
    std::fs::write(
        root.join("target.c"),
        original.replace("private int hidden = 5;", "private int hidden = 200;"),
    )
    .unwrap();
    let status = run(&mut vm, &admin).await;
    assert_eq!(status.state, "succeeded", "{}", status.error);
    vm.global_state.gc().await.unwrap().unwrap();
    assert_eq!(
        call(&vm, &admin, "check", &[]).await.unwrap(),
        LpcRef::from(1)
    );
}

#[tokio::test]
async fn invalid_targets_and_cleanup_changes_are_refused() {
    let (root, mut vm, admin) = setup("recompile-refusals").await;
    assert_eq!(
        call(&vm, &admin, "invalid_targets", &[]).await.unwrap(),
        LpcRef::from(1)
    );
    assert!(vm.next_op().is_none());
    std::fs::write(
        root.join("target.c"),
        format!("{UPDATED}\nint clean_up(int references) {{ return 0; }}"),
    )
    .unwrap();
    let status = run(&mut vm, &admin).await;
    assert_eq!(status.state, "failed");
    assert!(status.error.contains("cleanup eligibility"));
    assert_eq!(
        call(&vm, &admin, "sum", &[]).await.unwrap(),
        LpcRef::from(64)
    );
}

#[tokio::test]
async fn old_program_groups_and_inheritors_are_unchanged() {
    let (root, mut vm, admin) = setup("recompile-groups").await;
    let child = vm
        .initialize_process_from_code("/child.c", "inherit \"/target\";")
        .await
        .unwrap()
        .context
        .process;
    call(&vm, &admin, "destroy_target", &[]).await.unwrap();
    let newer = vm
        .initialize_process_from_code(
            "/newer.c",
            r#"
        object prototype = load_object("/target");
        object fresh = clone_object("/target");
        int request() { return request_object_recompile(prototype); }
        int value() { return fresh->value(); }
    "#,
        )
        .await
        .unwrap()
        .context
        .process;
    std::fs::write(root.join("target.c"), UPDATED).unwrap();
    let status = run(&mut vm, &newer).await;
    assert_eq!(status.state, "succeeded", "{}", status.error);
    assert_eq!(status.updated, 2);
    assert_eq!(
        call(&vm, &newer, "value", &[]).await.unwrap(),
        LpcRef::from(100)
    );
    assert_eq!(
        call(&vm, &admin, "dynamic_value", &[]).await.unwrap(),
        LpcRef::from(21)
    );
    assert_eq!(
        call(&vm, &admin, "pointers_fail", &[]).await.unwrap(),
        LpcRef::from(0)
    );
    assert_eq!(
        call(&vm, &child, "value", &[]).await.unwrap(),
        LpcRef::from(10)
    );
}

#[tokio::test]
async fn queued_callouts_survive_publication_and_validate_their_binding_when_fired() {
    let (root, mut vm, admin) = setup("recompile-callouts").await;
    call(&vm, &admin, "arm_callbacks", &[]).await.unwrap();
    let mut ids = vm.global_state.with_call_outs(|co| {
        co.queue()
            .iter()
            .map(|(_, entry)| entry.id)
            .collect::<Vec<_>>()
    });
    ids.sort_unstable();
    assert_eq!(ids.len(), 2);
    std::fs::write(root.join("target.c"), UPDATED).unwrap();
    assert_eq!(run(&mut vm, &admin).await.state, "succeeded");
    vm.global_state.with_call_outs(|co| {
        for id in &ids {
            assert!(co.get_by_id(*id).is_some());
        }
    });
    let first = vm.global_state.object_space.lookup("/target#0").unwrap();
    vm.global_state
        .prioritize_call_out(ids[0])
        .await
        .await
        .unwrap();
    assert_eq!(
        call(&vm, &first, "value", &[]).await.unwrap(),
        LpcRef::from(550)
    );
    vm.global_state
        .prioritize_call_out(ids[1])
        .await
        .await
        .unwrap();
    assert_eq!(
        call(&vm, &first, "value", &[]).await.unwrap(),
        LpcRef::from(770)
    );
    vm.global_state.with_call_outs(|co| assert!(co.is_empty()));
}

#[tokio::test]
async fn a_shadow_on_any_clone_refuses_the_group_upgrade() {
    let (root, mut vm, admin) = setup("recompile-shadow").await;
    let master = format!(
        "{PERMISSIVE_MASTER} int valid_recompile() {{ return 1; }} int query_allow_shadow() {{ return 1; }}"
    );
    vm.initialize_process_from_code("/secure/master.c", &master)
        .await
        .unwrap();
    vm.initialize_process_from_code(
        "/shadow.c",
        r#"
        void create() { shadow(find_object("/target#0")); }
        int value() { return 1234; }
    "#,
    )
    .await
    .unwrap();
    std::fs::write(root.join("target.c"), UPDATED).unwrap();
    let status = run(&mut vm, &admin).await;
    assert_eq!(status.state, "failed");
    assert!(status.error.contains("shadow chain"));
    let second = vm.global_state.object_space.lookup("/target#1").unwrap();
    assert_eq!(
        call(&vm, &second, "value", &[]).await.unwrap(),
        LpcRef::from(33)
    );
}

#[tokio::test]
async fn nested_efun_and_composed_applies_work_in_an_upgraded_owner() {
    use crate::interpreter::{
        apply::apply_pointer,
        function_type::{function_address::FunctionAddress, function_ptr::FunctionPtrBuilder},
    };
    let (root, mut vm, admin) = setup("recompile-driver-code").await;
    std::fs::write(root.join("target.c"), UPDATED).unwrap();
    assert_eq!(run(&mut vm, &admin).await.state, "succeeded");
    let target = vm.global_state.object_space.lookup("/target#0").unwrap();
    let LpcRef::Function(composed) = call(&vm, &target, "composed", &[]).await.unwrap() else {
        panic!("composed pointer");
    };
    let efun = Arc::new(
        FunctionPtrBuilder::default()
            .owner(Arc::downgrade(&target))
            .address(FunctionAddress::Efun(ustr::ustr("abs")))
            .build()
            .unwrap(),
    );
    let live = start_txn(&vm.global_state.committer_tx).await.unwrap();
    let mut context = TaskTemplate::from(vm.global_state.clone()).into_task_context(target.clone());
    context.txn = TxnHandle::new(Transaction::new(live.inner.clone()));
    for pointer in [efun, composed] {
        assert_eq!(
            apply_pointer(&context, &target, &pointer, &[(-5).into()])
                .await
                .unwrap(),
            Some(LpcRef::from(5))
        );
    }
}
