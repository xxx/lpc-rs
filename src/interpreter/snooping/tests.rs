use std::{sync::Arc, time::Duration};

use lpc_rs_core::null_mode::NullMode;
use lpc_rs_errors::Result;
use lpc_rs_utils::config::ConfigBuilder;

use super::*;
use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    interpreter::{
        lpc_ref::LpcRef,
        task::{SeedArg, SeedEntry, Task, TaskSeed, task_template::TaskTemplate},
        vm::Vm,
    },
    test_support::{Connected, connect},
};

const MASTER: &str = r#"
    int valid_snoop(object caller, object observer, object target, string operation, string program) { return 1; }
    int valid_destruct(object caller, object target, string program) { return 1; }
    int valid_exec(string program, object new, object old) { return 1; }
"#;

const OBSERVER: &str = r#"
    string text = "";
    int count;
    void receive_snoop(string s, object target, string direction) {
        if (this_player() || this_interactive() || previous_object() || origin() != "driver")
            throw("wrong callback context");
        text += direction + ":" + s;
        count++;
    }
    mixed run(object target) { return snoop(this_object(), target); }
    mixed stop() { return snoop(this_object()); }
    mixed query() { return query_snooping(this_object()); }
    mixed incoming() { return query_snoop(this_object()); }
    string result() { return text; }
    int hits() { return count; }
"#;

async fn call(vm: &Vm, process: &Arc<Process>, name: &str, args: Vec<LpcRef>) -> Result<LpcRef> {
    let context = TaskTemplate::from(vm.global_state.clone()).into_task_context(process.clone());
    let mut task = Task::<MAX_CALL_STACK_SIZE>::new(context);
    task.timed_eval_seed(
        TaskSeed {
            process: process.clone(),
            entry: SeedEntry::Named(name.into()),
            args: args.into_iter().map(SeedArg::Value).collect(),
            initializes: false,
        },
        5000,
    )
    .await?;
    Ok(task.result().unwrap_or(task.context.txn().nil()))
}

async fn object(vm: &Vm, name: &str, code: &str) -> Arc<Process> {
    vm.initialize_process_from_code(name, code)
        .await
        .unwrap()
        .context
        .process
}

async fn setup(
    mode: NullMode,
    limit: usize,
    observer_code: &str,
) -> (Vm, Arc<Process>, Arc<Process>, Connected) {
    let vm = Vm::new(
        ConfigBuilder::default()
            .lib_dir("./tests/fixtures/code")
            .null_mode(mode)
            .max_pending_snoop(limit)
            .build()
            .unwrap(),
    );
    object(&vm, "/secure/master.c", MASTER).await;
    let observer = object(&vm, "/observer.c", observer_code).await;
    let target = object(&vm, "/target.c", OBSERVER).await;
    let connected = connect(&vm, &target).await;
    (vm, observer, target, connected)
}

async fn attach(
    vm: &Vm,
    observer: &Arc<Process>,
    target: &Arc<Process>,
    connected: &mut Connected,
) -> Arc<Registration> {
    assert_eq!(
        call(
            vm,
            observer,
            "run",
            vec![LpcRef::from(Arc::downgrade(target))]
        )
        .await
        .unwrap(),
        LpcRef::from(Arc::downgrade(target))
    );
    let ConnectionOp::StartSnoop(registration) = connected.rx.try_recv().unwrap() else {
        panic!("start control")
    };
    registration
        .activate(&vm.global_state, &connected.connection)
        .await;
    registration
}

async fn until_text(vm: &Vm, observer: &Arc<Process>, expected: &str) {
    tokio::time::timeout(Duration::from_secs(3), async {
        loop {
            if call(vm, observer, "result", vec![])
                .await
                .unwrap()
                .to_string()
                == expected
            {
                break;
            }
            tokio::task::yield_now().await;
        }
    })
    .await
    .unwrap();
}

#[tokio::test]
async fn snoop_delivery_preserves_order_context_and_explicit_stop_in_both_null_modes() {
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let (vm, observer, target, mut connected) = setup(mode, 65536, OBSERVER).await;
        let registration = attach(&vm, &observer, &target, &mut connected).await;
        assert_eq!(
            call(&vm, &observer, "query", vec![]).await.unwrap(),
            LpcRef::from(Arc::downgrade(&target))
        );
        assert_eq!(
            call(&vm, &target, "incoming", vec![]).await.unwrap(),
            LpcRef::from(Arc::downgrade(&observer))
        );
        connected.connection.observe("one", "input", true);
        connected.connection.observe("two", "output", false);
        until_text(&vm, &observer, "input:one\noutput:two").await;
        call(
            &vm,
            &observer,
            "run",
            vec![LpcRef::from(Arc::downgrade(&target))],
        )
        .await
        .unwrap();
        assert!(
            connected.rx.try_recv().is_err(),
            "idempotent attach keeps the registration"
        );
        assert_eq!(
            call(&vm, &observer, "stop", vec![]).await.unwrap(),
            LpcRef::from(Arc::downgrade(&observer))
        );
        assert_eq!(
            call(&vm, &observer, "query", vec![]).await.unwrap(),
            LpcRef::nil(mode)
        );
        registration.capture("late", "input", true);
        assert_eq!(
            call(&vm, &observer, "hits", vec![]).await.unwrap(),
            LpcRef::from(2)
        );
        call(&vm, &observer, "stop", vec![]).await.unwrap();
    }
}

#[tokio::test]
async fn snoop_rejects_occupied_endpoints_cycles_and_incompatible_handlers() {
    let (vm, observer, target, mut connected) = setup(NullMode::Distinct, 65536, OBSERVER).await;
    attach(&vm, &observer, &target, &mut connected).await;
    let other = object(&vm, "/other.c", OBSERVER).await;
    let _other_connection = connect(&vm, &other).await;
    let _observer_connection = connect(&vm, &observer).await;
    assert!(
        call(
            &vm,
            &other,
            "run",
            vec![LpcRef::from(Arc::downgrade(&target))]
        )
        .await
        .unwrap_err()
        .to_string()
        .contains("already has")
    );
    assert!(
        call(
            &vm,
            &observer,
            "run",
            vec![LpcRef::from(Arc::downgrade(&other))]
        )
        .await
        .unwrap_err()
        .to_string()
        .contains("already attached")
    );
    assert!(
        call(
            &vm,
            &other,
            "run",
            vec![LpcRef::from(Arc::downgrade(&other))]
        )
        .await
        .unwrap_err()
        .to_string()
        .contains("cycle")
    );
    for (i, handler) in [
        "",
        "void receive_snoop(int a, object b, string c) {}",
        "void receive_snoop(string a) {}",
    ]
    .iter()
    .enumerate()
    {
        let bad = object(
            &vm,
            &format!("/bad{i}.c"),
            &format!("{handler} void run(object t) {{ snoop(this_object(), t); }}"),
        )
        .await;
        assert!(
            call(&vm, &bad, "run", vec![LpcRef::from(Arc::downgrade(&other))])
                .await
                .unwrap_err()
                .to_string()
                .contains("receive_snoop")
        );
    }
    assert!(
        call(
            &vm,
            &target,
            "run",
            vec![LpcRef::from(Arc::downgrade(&observer))]
        )
        .await
        .unwrap_err()
        .to_string()
        .contains("cycle")
    );
}

#[tokio::test]
async fn snoop_failure_and_overflow_retire_without_disconnect() {
    for (code, text) in [
        (
            OBSERVER.replace(
                "text += direction + \":\" + s;",
                "count++; throw(\"callback failed\");",
            ),
            "a",
        ),
        (OBSERVER.into(), "too long"),
    ] {
        let (vm, observer, target, mut connected) = setup(NullMode::Distinct, 4, &code).await;
        attach(&vm, &observer, &target, &mut connected).await;
        connected.connection.observe(text, "output", false);
        tokio::time::timeout(Duration::from_secs(3), async {
            while call(&vm, &observer, "query", vec![]).await.unwrap() != LpcRef::Nil {
                tokio::task::yield_now().await;
            }
        })
        .await
        .unwrap();
        assert_eq!(
            call(&vm, &observer, "hits", vec![]).await.unwrap(),
            LpcRef::from(0)
        );
        assert!(!connected.connection.is_dead());
    }
}

#[tokio::test]
async fn snoop_stale_controls_cannot_replace_or_stop_a_new_registration() {
    let (vm, observer, target, mut connected) = setup(NullMode::Distinct, 65536, OBSERVER).await;
    let old = attach(&vm, &observer, &target, &mut connected).await;
    call(&vm, &observer, "stop", vec![]).await.unwrap();
    assert!(matches!(
        connected.rx.try_recv(),
        Ok(ConnectionOp::StopSnoop(_))
    ));
    let new = attach(&vm, &observer, &target, &mut connected).await;
    assert_ne!(new.id, old.id);
    old.activate(&vm.global_state, &connected.connection).await;
    connected.connection.stop_snoop(old.id);
    connected.connection.observe("new", "output", false);
    until_text(&vm, &observer, "output:new").await;
}

#[tokio::test]
async fn snoop_disconnect_preserves_observer_but_retires_target() {
    let (vm, observer, target, mut connected) = setup(NullMode::Distinct, 65536, OBSERVER).await;
    let observer_connection = connect(&vm, &observer).await;
    attach(&vm, &observer, &target, &mut connected).await;
    vm.global_state
        .detach(&observer_connection.connection, None)
        .await;
    assert_eq!(
        call(&vm, &observer, "query", vec![]).await.unwrap(),
        LpcRef::from(Arc::downgrade(&target))
    );
    vm.global_state.detach(&connected.connection, None).await;
    assert_eq!(
        call(&vm, &observer, "query", vec![]).await.unwrap(),
        LpcRef::Nil
    );
}

#[tokio::test]
async fn snoop_authorization_is_required_for_start_stop_and_live_queries() {
    for policy in [
        "",
        "int valid_snoop(object c, object o, object t, string op, string p) { return 0; }",
        "int valid_snoop(object c, object o, object t, string op, string p) { throw(\"policy error\"); }",
    ] {
        let vm = Vm::new(
            ConfigBuilder::default()
                .lib_dir("./tests/fixtures/code")
                .build()
                .unwrap(),
        );
        object(&vm, "/secure/master.c", policy).await;
        let observer = object(&vm, "/observer.c", OBSERVER).await;
        let target = object(&vm, "/target.c", "").await;
        let _connection = connect(&vm, &target).await;
        for (name, args) in [
            ("run", vec![LpcRef::from(Arc::downgrade(&target))]),
            ("stop", vec![]),
            ("query", vec![]),
        ] {
            let error = call(&vm, &observer, name, args)
                .await
                .unwrap_err()
                .to_string();
            assert!(
                error.contains(if policy.contains("throw") {
                    "policy error"
                } else {
                    "permission denied"
                }),
                "{error}"
            );
        }
    }
}

#[tokio::test]
async fn snoop_authorization_arguments_and_query_absence() {
    let vm = Vm::new(
        ConfigBuilder::default()
            .lib_dir("./tests/fixtures/code")
            .null_mode(NullMode::Distinct)
            .build()
            .unwrap(),
    );
    object(&vm, "/secure/master.c", r#"
        int valid_snoop(object caller, object observer, object target, string operation, string program) {
            if (caller != find_object("/observer") || program != "/observer.c") throw("caller");
            if (operation == "query_snoop") { if (observer || target != caller) throw("query target"); }
            else if (observer != caller) throw("observer");
            if (operation == "query_snooping" && target) throw("query observer");
            return 1;
        }
    "#).await;
    let observer = object(
        &vm,
        "/observer.c",
        &format!(
            r#"{OBSERVER}
        int absent() {{ return query_snoop(nil) == nil && query_snooping(nil) == nil; }}
        void zero() {{ mixed z = 0; query_snoop(z); }}
    "#
        ),
    )
    .await;
    assert_eq!(
        call(&vm, &observer, "incoming", vec![]).await.unwrap(),
        LpcRef::Nil
    );
    assert_eq!(
        call(&vm, &observer, "query", vec![]).await.unwrap(),
        LpcRef::Nil
    );
    assert_eq!(
        call(&vm, &observer, "absent", vec![]).await.unwrap(),
        LpcRef::from(1)
    );
    assert!(call(&vm, &observer, "zero", vec![]).await.is_err());
    call(&vm, &observer, "stop", vec![]).await.unwrap();
}

#[tokio::test]
async fn snoop_abort_and_attach_output_stop_publish_no_capture_window() {
    let code = format!(
        r#"{OBSERVER}
        void abort_start(object t) {{ snoop(this_object(), t); throw("abort"); }}
        void transient(object t) {{ snoop(this_object(), t); tell_object(t, "between"); snoop(this_object()); }}
    "#
    );
    let (vm, observer, target, mut connected) = setup(NullMode::Distinct, 65536, &code).await;
    assert!(
        call(
            &vm,
            &observer,
            "abort_start",
            vec![LpcRef::from(Arc::downgrade(&target))]
        )
        .await
        .is_err()
    );
    assert!(connected.rx.try_recv().is_err());
    assert_eq!(
        call(&vm, &observer, "query", vec![]).await.unwrap(),
        LpcRef::Nil
    );
    call(
        &vm,
        &observer,
        "transient",
        vec![LpcRef::from(Arc::downgrade(&target))],
    )
    .await
    .unwrap();
    while let Ok(op) = connected.rx.try_recv() {
        match op {
            ConnectionOp::StartSnoop(r) => {
                r.activate(&vm.global_state, &connected.connection).await
            }
            ConnectionOp::StopSnoop(id) => connected.connection.stop_snoop(id),
            ConnectionOp::SendMessage(text) => connected.connection.observe(&text, "output", false),
            _ => panic!("unexpected op"),
        }
    }
    assert_eq!(
        call(&vm, &observer, "hits", vec![]).await.unwrap(),
        LpcRef::from(0)
    );
}

#[tokio::test]
async fn snoop_destroy_and_exec_retire_both_ends_and_replacement_binding() {
    for operation in [
        "destruct(find_object(\"/target\"));",
        "destruct(find_object(\"/observer\"));",
        "exec(this_object(), find_object(\"/target\"));",
    ] {
        let (vm, observer, target, mut connected) =
            setup(NullMode::Distinct, 65536, OBSERVER).await;
        let registration = attach(&vm, &observer, &target, &mut connected).await;
        object(
            &vm,
            "/action.c",
            &format!("void create() {{ {operation} }}"),
        )
        .await;
        let live = crate::interpreter::stm::start_txn(&vm.global_state.committer_tx)
            .await
            .unwrap();
        let mut txn = Transaction::new(live.inner.clone());
        assert!(!registration.valid(&mut txn));
        assert!(incoming(&mut txn, &target).is_none());
        assert!(outgoing(&mut txn, &observer).is_none());
    }
    let (vm, observer, target, mut connected) = setup(NullMode::Distinct, 65536, OBSERVER).await;
    attach(&vm, &observer, &target, &mut connected).await;
    let _replacement = connect(&vm, &target).await;
    assert_eq!(
        call(&vm, &observer, "query", vec![]).await.unwrap(),
        LpcRef::Nil
    );
}

#[tokio::test]
async fn snoop_conflicting_cycles_and_revoked_delivery_reads_cannot_commit() {
    let (vm, observer, target, mut connected) = setup(NullMode::Distinct, 65536, OBSERVER).await;
    let _observer_connection = connect(&vm, &observer).await;
    let live = crate::interpreter::stm::start_txn(&vm.global_state.committer_tx)
        .await
        .unwrap();
    let first = TxnHandle::new(Transaction::new(live.inner.clone()));
    let second = TxnHandle::new(Transaction::new(live.inner.clone()));
    start(&first, &observer, &target, 65536).unwrap();
    start(&second, &target, &observer, 65536).unwrap();
    let take = |handle: &TxnHandle| {
        handle.with(|t| std::mem::replace(t, Transaction::new(live.inner.clone())))
    };
    let (result, effects) = take(&first)
        .commit(&vm.global_state.committer_tx)
        .await
        .unwrap();
    result.unwrap();
    crate::interpreter::stm::flush_effects(&vm.global_state, effects).await;
    assert!(
        take(&second)
            .commit(&vm.global_state.committer_tx)
            .await
            .unwrap()
            .0
            .is_err()
    );
    let ConnectionOp::StartSnoop(registration) = connected.rx.try_recv().unwrap() else {
        panic!("start")
    };
    let delivery_live = crate::interpreter::stm::start_txn(&vm.global_state.committer_tx)
        .await
        .unwrap();
    let mut delivery = Transaction::new(delivery_live.inner.clone());
    assert!(registration.valid(&mut delivery));
    call(&vm, &observer, "stop", vec![]).await.unwrap();
    assert!(
        delivery
            .commit(&vm.global_state.committer_tx)
            .await
            .unwrap()
            .0
            .is_err()
    );
}

#[tokio::test]
async fn snoop_authorization_rechecks_destroyed_endpoints() {
    let (vm, observer, target, _connected) = setup(NullMode::Distinct, 65536, OBSERVER).await;
    // A replacement master destroys the target inside the authorization apply.
    object(&vm, "/secure/master.c", r#"
        int valid_destruct(object c, object t, string p) { return 1; }
        int valid_snoop(object c, object o, object t, string op, string p) { if (t) destruct(t); return 1; }
    "#).await;
    assert!(
        call(
            &vm,
            &observer,
            "run",
            vec![LpcRef::from(Arc::downgrade(&target))]
        )
        .await
        .unwrap_err()
        .to_string()
        .contains("destroyed")
    );
}

#[tokio::test]
async fn snoop_registration_survives_gc() {
    let (vm, observer, target, mut connected) = setup(NullMode::Distinct, 65536, OBSERVER).await;
    attach(&vm, &observer, &target, &mut connected).await;
    vm.global_state.gc().await.unwrap().unwrap();
    connected.connection.observe("after gc", "output", false);
    until_text(&vm, &observer, "output:after gc").await;
}

#[tokio::test]
async fn snoop_rejects_dead_noninteractive_and_closing_endpoints() {
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let (vm, observer, target, connected) = setup(mode, 65536, OBSERVER).await;
        let other = object(&vm, "/other.c", "").await;
        let error = call(
            &vm,
            &observer,
            "run",
            vec![LpcRef::from(Arc::downgrade(&other))],
        )
        .await
        .unwrap_err();
        assert!(error.to_string().contains("not interactive"));
        let controller = object(&vm, "/controller.c", r#"
            void dead() { object t = find_object("/other"); destruct(t); snoop(find_object("/observer"), t); }
            void absent() { snoop(nil, this_object()); }
            int query_dead() { object t = find_object("/other"); destruct(t); return query_snoop(t) == nil && query_snooping(t) == nil; }
        "#).await;
        assert!(
            call(&vm, &controller, "dead", vec![])
                .await
                .unwrap_err()
                .to_string()
                .contains("not live")
        );
        assert!(
            call(&vm, &controller, "absent", vec![])
                .await
                .unwrap_err()
                .to_string()
                .contains("not live")
        );
        assert_eq!(
            call(&vm, &controller, "query_dead", vec![]).await.unwrap(),
            LpcRef::from(1)
        );
        connected.connection.mark_dead();
        assert!(
            call(
                &vm,
                &observer,
                "run",
                vec![LpcRef::from(Arc::downgrade(&target))]
            )
            .await
            .unwrap_err()
            .to_string()
            .contains("not interactive")
        );
    }
}

#[tokio::test]
async fn snoop_recompile_uses_new_handler_and_retires_an_incompatible_one() {
    use crate::test_support::{PERMISSIVE_MASTER, lib_holding, temp_lib_config};
    let master = format!(
        "{PERMISSIVE_MASTER} int valid_snoop(object c, object o, object t, string op, string p) {{ return 1; }} int valid_recompile() {{ return 1; }}"
    );
    let root = lib_holding(
        "snoop-recompile",
        &[("secure/master.c", &master), ("observer.c", OBSERVER)],
    );
    let mut vm = Vm::new(temp_lib_config(&root));
    object(&vm, "/secure/master.c", &master).await;
    let observer = object(&vm, "/observer.c", OBSERVER).await;
    let target = object(&vm, "/target.c", "").await;
    let admin = object(
        &vm,
        "/admin.c",
        "int request() { return request_object_recompile(find_object(\"/observer\")); }",
    )
    .await;
    let mut connected = connect(&vm, &target).await;
    attach(&vm, &observer, &target, &mut connected).await;
    for compatible in [true, false] {
        let code = if compatible {
            OBSERVER.replace("text += direction + \":\" + s;", "text += \"new:\" + s;")
        } else {
            OBSERVER
                .replace(
                    "string s, object target, string direction",
                    "int s, object target, string direction",
                )
                .replace("text += direction + \":\" + s;", "text = direction;")
        };
        std::fs::write(root.join("observer.c"), code).unwrap();
        call(&vm, &admin, "request", vec![]).await.unwrap();
        let Some(crate::interpreter::vm::vm_op::VmOp::ObjectUpdate(request)) = vm.next_op() else {
            panic!("recompile queued")
        };
        vm.global_state.run_object_update(request.clone()).await;
        let status = vm.global_state.updates.get(request.id).unwrap();
        assert_eq!(status.state, "succeeded", "{}", status.error);
        connected.connection.observe("hello", "output", false);
        if compatible {
            until_text(&vm, &observer, "new:hello").await;
        } else {
            tokio::time::timeout(Duration::from_secs(3), async {
                while call(&vm, &observer, "query", vec![]).await.unwrap()
                    != LpcRef::nil(vm.global_state.config.null_mode)
                {
                    tokio::task::yield_now().await;
                }
            })
            .await
            .unwrap();
            assert_eq!(
                call(&vm, &observer, "hits", vec![]).await.unwrap(),
                LpcRef::from(1)
            );
        }
    }
}
