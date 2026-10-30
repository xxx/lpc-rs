use lpc_rs_core::null_mode::NullMode;
use lpc_rs_utils::config::ConfigBuilder;
use tokio::{
    io::{AsyncReadExt, AsyncWriteExt},
    net::TcpStream,
    time::{Duration, timeout},
};

use super::*;
use crate::{
    interpreter::{
        stm::{Transaction, commit_changeset, start_txn},
        task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
        task_context::TaskContext,
        tests::{run, run_with},
        vm::Vm,
    },
    test_support::{PERMISSIVE_MASTER, TempLib, lib_holding},
};

const ALLOW: &str = "int valid_socket(object caller, string op, mixed *info) { return 1; }";
const OWNER: &str = r#"
    int handle;
    void event(int h, string event, mixed data) {}
    void offered(int h, object previous) {}
    int open() { return handle = socket_connect("localhost", 9000, event); }
    void cancel(int h) { socket_close(h); }
    int write(int h) { return socket_write(h, to_bytes("abc", "UTF-8")); }
    mapping info(int h) { return socket_info(h); }
    object owner() { return socket_info(handle)["owner"]; }
    int give(string name) {
        object recipient = find_object("/recipient");
        return socket_release(handle, recipient, recipient->offer(name));
    }
    int caught_give(string name) { return stringp(catch(give(name))); }
    string state() { return socket_info(handle)["state"]; }
    int acquire_without_offer() { return stringp(catch(socket_acquire(handle, event))); }
    int write_as_foreign(int h) { return stringp(catch(socket_write(h, to_bytes("bad", "UTF-8")))); }
    int close_as_foreign(int h) { return stringp(catch(socket_close(h))); }
"#;
const RECIPIENT: &str = r#"
    int accepted;
    int context_result;
    int scopes_ok;
    void event(int h, string event, mixed data) {}
    private void yes(int h, object previous) {
        context_result = origin() == "efun" && previous_object() == previous
            && this_player() == previous && this_interactive() == nil;
        socket_acquire(h, event); accepted = h;
    }
    void no(int h, object previous) {}
    void before_error(int h, object previous) { throw("before acquisition"); }
    void after_error(int h, object previous) { socket_acquire(h, event); accepted = h; throw("after acquisition"); }
    void scope(int h, object previous) {
        scopes_ok = previous->acquire_without_offer()
            && stringp(catch(socket_release(h, previous, &previous->offered())))
            && stringp(catch(socket_acquire(h + 1, event)));
        socket_acquire(h, event);
        scopes_ok = scopes_ok && stringp(catch(socket_acquire(h, event)));
    }
    function offer(string name) {
        switch (name) {
            case "yes": return yes;
            case "no": return no;
            case "before": return before_error;
            case "after": return after_error;
            case "scope": return scope;
            case "closure": {
                int token = 42;
                return (: [int h, object previous] socket_acquire(h, event); accepted = token :);
            }
        }
        return no;
    }
    int received() { return accepted; }
    int context_ok() { return context_result; }
    int scope_ok() { return scopes_ok; }
    int later(int h) { return stringp(catch(socket_acquire(h, event))); }
"#;

struct Fixture {
    vm: Vm,
    _root: TempLib,
    owner: Arc<Process>,
    recipient: Arc<Process>,
}

impl Fixture {
    async fn new(policy: &str, owner: &str, recipient: &str) -> Self {
        Self::with_output_limit(policy, owner, recipient, 65536).await
    }

    async fn with_output_limit(policy: &str, owner: &str, recipient: &str, limit: usize) -> Self {
        let master = format!("{PERMISSIVE_MASTER}\n{policy}");
        let root = lib_holding(
            "socket-efuns",
            &[
                ("secure/master.c", &master),
                ("owner.c", owner),
                ("recipient.c", recipient),
            ],
        );
        let vm = Vm::new(
            ConfigBuilder::default()
                .lib_dir(root.to_str().unwrap())
                .null_mode(NullMode::Distinct)
                .max_execution_time(4000u64)
                .max_pending_socket_output(limit)
                .build()
                .unwrap(),
        );
        vm.initialize_process_from_code("/secure/master.c", &master)
            .await
            .unwrap();
        let owner = vm
            .initialize_process_from_code("/owner.c", owner)
            .await
            .unwrap_or_else(|e| panic!("{}", e.diagnostic_string()))
            .context
            .process;
        let recipient = vm
            .initialize_process_from_code("/recipient.c", recipient)
            .await
            .unwrap_or_else(|e| panic!("{}", e.diagnostic_string()))
            .context
            .process;
        Self {
            vm,
            _root: root,
            owner,
            recipient,
        }
    }
    async fn call(&self, process: &Arc<Process>, name: &str, args: &[LpcRef]) -> Result<LpcRef> {
        apply_function_by_name(
            name,
            args,
            process.clone(),
            TaskTemplate::from(self.vm.global_state.clone()),
            None,
        )
        .await
        .unwrap()
    }
    async fn established(&self) -> SocketHandle {
        let value = self.call(&self.owner, "open", &[]).await.unwrap();
        let LpcRef::Int(h) = value else {
            panic!("handle")
        };
        let handle = SocketHandle::try_from(h.0).unwrap();
        self.publish_ready(
            handle,
            "127.0.0.1:1000".parse().unwrap(),
            Some("127.0.0.1:9000".parse().unwrap()),
        )
        .await;
        handle
    }

    async fn publish_ready(
        &self,
        handle: SocketHandle,
        local: SocketAddr,
        remote: Option<SocketAddr>,
    ) {
        let mut live = start_txn(&self.vm.global_state.committer_tx).await.unwrap();
        let mut ctx = TaskContext::new(self.vm.global_state.clone(), self.owner.clone(), None);
        ctx.txn = TxnHandle::new(Transaction::new(live.inner.clone()));
        SocketAccess::new(&ctx)
            .ready(handle, local, remote)
            .unwrap();
        live.disarm();
        commit_changeset(
            &self.vm.global_state.committer_tx,
            ctx.txn.with(Transaction::take_changeset),
        )
        .await
        .unwrap()
        .unwrap();
    }
}

#[tokio::test]
async fn known_stream_and_listener_endpoints_reach_permission_hooks() {
    let owner = format!(
        r#"{OWNER}
        int bind() {{ return handle = socket_listen("127.0.0.1", 0, event); }}
    "#
    );
    for (open, local, remote, port) in [
        ("open", "127.0.0.1:1000", Some("127.0.0.1:9000"), 9000),
        ("bind", "127.0.0.1:7000", None, 7000),
    ] {
        let policy = format!(
            r#"
            int valid_socket(object caller, string op, mixed *info) {{
                if (op == "connect" || op == "listen") return 1;
                return info[2] == "127.0.0.1" && info[3] == {port};
            }}
        "#
        );
        let fixture = Fixture::new(&policy, &owner, RECIPIENT).await;
        let LpcRef::Int(value) = fixture.call(&fixture.owner, open, &[]).await.unwrap() else {
            panic!("socket handle");
        };
        let handle = SocketHandle::try_from(value.0).unwrap();
        fixture
            .publish_ready(
                handle,
                local.parse().unwrap(),
                remote.map(|a| a.parse().unwrap()),
            )
            .await;
        let args = [handle.get().into()];
        assert!(matches!(
            fixture.call(&fixture.owner, "info", &args).await.unwrap(),
            LpcRef::Mapping(_)
        ));
        if remote.is_some() {
            assert_eq!(
                fixture.call(&fixture.owner, "write", &args).await.unwrap(),
                LpcRef::from(1)
            );
        }
        fixture.call(&fixture.owner, "cancel", &args).await.unwrap();
    }
}

#[tokio::test]
async fn creation_inspection_and_query_use_native_nil_and_transaction_snapshots() {
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let rows = run_with(ConfigBuilder::default().lib_dir("tests/fixtures/code").null_mode(mode).build().unwrap(), r#"
            int valid_socket(object caller, string op, mixed *info) {
                return op != "query" || info[0] == -1;
            }
        "#, &[], r#"
            void event(int h, string event, mixed data) {}
            mixed *create() {
                int stream = socket_connect("localhost", 9000, event);
                int listener = socket_listen("::1", 0, event);
                mapping info = socket_info(stream);
                socket_write(stream, to_bytes(({ 65, 0, 66 })));
                mapping current = socket_info(stream);
                return ({ stream > 0 && listener > stream,
                    info["owner"] == this_object() && info["kind"] == "stream",
                    info["state"] == "connecting" && info["local"] == nil && info["remote"] == nil,
                    info["output_pending"] == 0 && current["output_pending"] == 3,
                    socket_info(listener)["state"] == "binding", sizeof(query_sockets()),
                    sizeof(query_sockets(this_object())), sizeof(query_sockets(nil)), sizeof(query_sockets(-1)),
                    socket_info(99999) == nil });
            }
        "#).await;
        assert_eq!(
            rows,
            vec![
                LpcRef::from(1),
                LpcRef::from(1),
                LpcRef::from(1),
                LpcRef::from(1),
                LpcRef::from(1),
                LpcRef::from(2),
                LpcRef::from(2),
                LpcRef::from(2),
                LpcRef::from(2),
                LpcRef::from(1)
            ]
        );
    }
}

#[tokio::test]
async fn missing_or_refusing_policy_denies_creation_and_inspection_without_commands() {
    for policy in [
        "",
        "int valid_socket(object caller, string op, mixed *info) { return 0; }",
    ] {
        let f = Fixture::new(
            policy,
            r#"
            void event(int h, string event, mixed data) {}
            mixed *probe() { return ({ stringp(catch(socket_connect("localhost", 80, event))),
                stringp(catch(socket_listen("127.0.0.1", 0, event))),
                stringp(catch(socket_info(1))), stringp(catch(query_sockets())) }); }
        "#,
            "",
        )
        .await;
        let result = f.call(&f.owner, "probe", &[]).await.unwrap();
        let mut ctx = TaskContext::new(f.vm.global_state.clone(), f.owner.clone(), None);
        let live = start_txn(&f.vm.global_state.committer_tx).await.unwrap();
        ctx.txn = TxnHandle::new(Transaction::new(live.inner.clone()));
        result
            .with_array(&ctx.txn, |a| {
                assert!(a.iter().all(|v| v == &LpcRef::from(1)))
            })
            .unwrap();
        assert!(
            f.vm.global_state
                .sockets
                .take_commands()
                .unwrap()
                .is_empty()
        );
    }
}

#[tokio::test]
async fn policy_gets_the_executing_owner_request_and_preserved_player_context() {
    let rows = run(
        r#"
        int calls;
        int valid_socket(object caller, string op, mixed *info) {
            calls++;
            return this_player() == caller && previous_object() == caller
                && sizeof(info) == 5 && info[0] == -1 && info[1] == caller
                && info[2] == "localhost" && info[3] == 9000 && info[4] == nil
                && op == "connect";
        }
        int calls() { return calls; }
    "#,
        &[],
        r#"
        void event(int h, string event, mixed data) {}
        mixed *create() { enable_commands(); set_this_player(this_object());
            return ({ socket_connect("localhost", 9000, event) > 0,
                stringp(catch(query_sockets())), find_object("/secure/master")->calls() }); }
    "#,
    )
    .await;
    assert_eq!(
        rows,
        vec![LpcRef::from(1), LpcRef::from(1), LpcRef::from(2)]
    );
}

#[tokio::test]
async fn shape_errors_and_dead_query_selectors_do_not_become_global_inspection() {
    let master = format!("{PERMISSIVE_MASTER}\n{ALLOW}");
    let rows = run(
        &master,
        &[("/other.c", "")],
        r#"
        void event(int h, string event, mixed data) {}
        mixed *create() {
            object dead = find_object("/other"); destruct(dead);
            return ({ stringp(catch(socket_connect("localhost", 0, event))),
                stringp(catch(socket_connect("", 80, event))),
                stringp(catch(socket_connect(sprintf("local%chost", 0), 80, event))),
                stringp(catch(socket_connect("localhost", 65536, event))),
                stringp(catch(socket_listen("localhost", 1, event))),
                stringp(catch(socket_listen("::1", -1, event))),
                stringp(catch(socket_info(0))), stringp(catch(query_sockets(dead))),
                stringp(catch(query_sockets(2))),
                stringp(catch(socket_acquire(1, event))) });
        }
    "#,
    )
    .await;
    assert_eq!(rows, vec![LpcRef::from(1); 10]);
}

#[tokio::test]
async fn authorization_cannot_create_for_a_caller_it_destructed() {
    let f = Fixture::new(
        r#"
        int valid_socket(object caller, string op, mixed *info) { destruct(caller); return 1; }
    "#,
        OWNER,
        "",
    )
    .await;
    let error = f.call(&f.owner, "open", &[]).await.unwrap_err();
    assert!(error.to_string().contains("caller is not live"));
    assert!(
        f.vm.global_state
            .sockets
            .take_commands()
            .unwrap()
            .is_empty()
    );
}

#[tokio::test]
async fn writes_recheck_close_made_by_the_permission_hook() {
    let f = Fixture::new(
        r#"
        int valid_socket(object caller, string op, mixed *info) {
            if (op == "write") caller->cancel(info[0]); return 1;
        }
    "#,
        OWNER,
        "",
    )
    .await;
    let h = f.established().await;
    assert!(
        f.call(&f.owner, "write", &[LpcRef::from(h.get())])
            .await
            .unwrap_err()
            .to_string()
            .contains("not writable")
    );
    assert_eq!(
        f.call(&f.owner, "state", &[]).await.unwrap().as_str(),
        Some("open")
    );
    // The uncaught write error aborts the permission hook's close too.
    let commands = f.vm.global_state.sockets.take_commands().unwrap();
    assert_eq!(
        commands
            .try_iter()
            .flatten()
            .filter(|c| matches!(
                c,
                crate::interpreter::sockets::SocketCommand::Close { .. }
                    | crate::interpreter::sockets::SocketCommand::Write { .. }
            ))
            .count(),
        0
    );
}

#[tokio::test]
async fn foreign_mutations_are_rejected_even_when_the_master_allows_inspection() {
    let f = Fixture::new(ALLOW, OWNER, OWNER).await;
    let h = f.established().await;
    for name in ["write_as_foreign", "close_as_foreign"] {
        assert_eq!(
            f.call(&f.recipient, name, &[LpcRef::from(h.get())])
                .await
                .unwrap(),
            LpcRef::from(1)
        );
    }
    assert_eq!(
        f.call(&f.recipient, "info", &[LpcRef::from(h.get())])
            .await
            .unwrap()
            .type_name(),
        "mapping"
    );
}

#[tokio::test]
async fn consent_transfers_inventory_and_private_callback_with_efun_context() {
    let owner = format!(
        "{OWNER}\nint give_as_player() {{ enable_commands(); set_this_player(this_object()); return give(\"yes\"); }}"
    );
    let f = Fixture::new(ALLOW, &owner, RECIPIENT).await;
    let h = f.established().await;
    assert_eq!(
        f.call(&f.owner, "give_as_player", &[]).await.unwrap(),
        LpcRef::from(1)
    );
    assert_eq!(
        f.call(&f.owner, "owner", &[]).await.unwrap(),
        LpcRef::from(Arc::downgrade(&f.recipient))
    );
    assert_eq!(
        f.call(&f.recipient, "received", &[]).await.unwrap(),
        LpcRef::from(h.get())
    );
    assert_eq!(
        f.call(&f.recipient, "context_ok", &[]).await.unwrap(),
        LpcRef::from(1)
    );
    assert_eq!(
        f.call(&f.recipient, "later", &[LpcRef::from(h.get())])
            .await
            .unwrap(),
        LpcRef::from(1)
    );
    assert!(
        f.call(&f.owner, "write", &[LpcRef::from(h.get())])
            .await
            .unwrap_err()
            .to_string()
            .contains("wrong owner")
    );
}

#[tokio::test]
async fn offer_refusal_errors_and_caught_acquisition_follow_shared_transaction_semantics() {
    let f = Fixture::new(ALLOW, OWNER, RECIPIENT).await;
    let h = f.established().await;
    assert_eq!(
        f.call(&f.owner, "give", &["no".into()]).await.unwrap(),
        LpcRef::from(0)
    );
    assert_eq!(
        f.call(&f.owner, "caught_give", &["before".into()])
            .await
            .unwrap(),
        LpcRef::from(1)
    );
    assert_eq!(
        f.call(&f.owner, "owner", &[]).await.unwrap(),
        LpcRef::from(Arc::downgrade(&f.owner))
    );
    let error = f
        .call(&f.owner, "give", &["after".into()])
        .await
        .unwrap_err();
    assert!(error.to_string().contains("after acquisition"));
    assert_eq!(
        f.call(&f.owner, "owner", &[]).await.unwrap(),
        LpcRef::from(Arc::downgrade(&f.owner))
    );
    assert_eq!(
        f.call(&f.recipient, "received", &[]).await.unwrap(),
        LpcRef::from(0)
    );
    assert_eq!(
        f.call(&f.owner, "caught_give", &["after".into()])
            .await
            .unwrap(),
        LpcRef::from(1)
    );
    assert_eq!(
        f.call(&f.owner, "owner", &[]).await.unwrap(),
        LpcRef::from(Arc::downgrade(&f.recipient))
    );
    assert_eq!(
        f.call(&f.recipient, "received", &[]).await.unwrap(),
        LpcRef::from(h.get())
    );
}

#[tokio::test]
async fn offer_scope_rejects_third_objects_recursive_offers_wrong_handles_and_repeat_acquisition() {
    let f = Fixture::new(ALLOW, OWNER, RECIPIENT).await;
    f.established().await;
    assert_eq!(
        f.call(&f.owner, "give", &["scope".into()]).await.unwrap(),
        LpcRef::from(1)
    );
    assert_eq!(
        f.call(&f.recipient, "scope_ok", &[]).await.unwrap(),
        LpcRef::from(1)
    );
}

#[tokio::test]
async fn an_offer_cannot_acquire_another_live_stream_of_the_previous_owner() {
    let recipient = r#"
        void event(int h, string event, mixed data) {}
        void wrong(int h, object previous) { socket_acquire(h - 1, event); }
        function offer(string name) { return wrong; }
    "#;
    let fixture = Fixture::new(ALLOW, OWNER, recipient).await;
    let first = fixture.established().await;
    let offered = fixture.established().await;
    assert_eq!(offered.get(), first.get() + 1);
    assert!(
        fixture
            .call(&fixture.owner, "give", &["wrong".into()])
            .await
            .unwrap_err()
            .to_string()
            .contains("invalid offer context")
    );
    assert_eq!(
        fixture.call(&fixture.owner, "owner", &[]).await.unwrap(),
        LpcRef::from(Arc::downgrade(&fixture.owner))
    );
}

#[tokio::test]
async fn offer_closures_keep_captures_and_type_checks() {
    let f = Fixture::new(ALLOW, OWNER, RECIPIENT).await;
    f.established().await;
    assert_eq!(
        f.call(&f.owner, "give", &["closure".into()]).await.unwrap(),
        LpcRef::from(1)
    );
    assert_eq!(
        f.call(&f.recipient, "received", &[]).await.unwrap(),
        LpcRef::from(42)
    );
}

#[tokio::test]
async fn admission_rechecks_callback_receivers_destructed_by_authorization() {
    let f = Fixture::new(r#"
        int valid_socket(object caller, string op, mixed *info) {
            if (op == "connect") destruct(find_object("/recipient")); return 1;
        }
    "#, r#"
        int open() { return socket_connect("localhost", 9000, &(find_object("/recipient"))->event()); }
    "#, "void event(int h, string event, mixed data) {}").await;
    assert!(
        f.call(&f.owner, "open", &[])
            .await
            .unwrap_err()
            .to_string()
            .contains("callback is not live")
    );
    assert!(
        f.vm.global_state
            .sockets
            .take_commands()
            .unwrap()
            .is_empty()
    );
}

#[tokio::test]
async fn permission_exceptions_propagate_without_admitting_a_socket() {
    let f = Fixture::new(
        r#"
        int valid_socket(object caller, string op, mixed *info) { throw("policy failure"); }
    "#,
        OWNER,
        "",
    )
    .await;
    assert!(
        f.call(&f.owner, "open", &[])
            .await
            .unwrap_err()
            .to_string()
            .contains("policy failure")
    );
    assert!(
        f.vm.global_state
            .sockets
            .take_commands()
            .unwrap()
            .is_empty()
    );
}

#[tokio::test]
async fn inspection_rechecks_an_owner_changed_during_authorization() {
    let owner = format!(
        r#"{OWNER}
        int caught_info(int h) {{ return stringp(catch(socket_info(h))); }}
    "#
    );
    let f = Fixture::new(
        r#"
        int valid_socket(object caller, string op, mixed *info) {
            if (op == "info" && info[1] == find_object("/owner")) info[1]->give("yes");
            return 1;
        }
    "#,
        &owner,
        RECIPIENT,
    )
    .await;
    let h = f.established().await;
    assert!(
        f.call(&f.owner, "info", &[LpcRef::from(h.get())])
            .await
            .unwrap_err()
            .to_string()
            .contains("ownership changed")
    );
    assert_eq!(
        f.call(&f.recipient, "received", &[]).await.unwrap(),
        LpcRef::from(0)
    );
    assert_eq!(
        f.call(&f.owner, "caught_info", &[LpcRef::from(h.get())])
            .await
            .unwrap(),
        LpcRef::from(1)
    );
    assert_eq!(
        f.call(&f.recipient, "received", &[]).await.unwrap(),
        LpcRef::from(h.get())
    );
    assert!(
        f.call(&f.owner, "write", &[LpcRef::from(h.get())])
            .await
            .unwrap_err()
            .to_string()
            .contains("wrong owner")
    );
}

#[tokio::test]
async fn whole_buffer_admission_empty_writes_and_close_are_authorized_and_ordered() {
    let f = Fixture::with_output_limit(
        r#"
        int valid_socket(object caller, string op, mixed *info) { return op != "info"; }
    "#,
        r#"
        void event(int h, string name, mixed data) {}
        int probe() {
            int h = socket_connect("localhost", 9000, event);
            bytes data = to_bytes("x" * 8, "UTF-8");
            int first = socket_write(h, data);
            int full = !socket_write(h, to_bytes("y", "UTF-8"));
            int empty = socket_write(h, to_bytes("", "UTF-8"));
            int too_big = stringp(catch(socket_write(h, data + to_bytes("z", "UTF-8"))));
            socket_close(h); socket_close(h);
            int no_late = stringp(catch(socket_write(h, to_bytes("late", "UTF-8"))));
            return first && full && empty && too_big && no_late
                && stringp(catch(socket_info(h)));
        }
    "#,
        "",
        8,
    )
    .await;
    assert_eq!(
        f.call(&f.owner, "probe", &[]).await.unwrap(),
        LpcRef::from(1)
    );
    let commands: Vec<_> =
        f.vm.global_state
            .sockets
            .take_commands()
            .unwrap()
            .try_iter()
            .flatten()
            .collect();
    assert_eq!(commands.len(), 3);
    assert!(matches!(
        commands[0],
        crate::interpreter::sockets::SocketCommand::Open { .. }
    ));
    assert!(
        matches!(&commands[1], crate::interpreter::sockets::SocketCommand::Write { data, .. } if data.len() == 8)
    );
    assert!(matches!(
        commands[2],
        crate::interpreter::sockets::SocketCommand::Close { drain: false, .. }
    ));
}

#[tokio::test]
async fn release_and_acquire_denial_leave_ownership_and_inventories_unchanged() {
    for denied in ["release", "acquire"] {
        let policy = format!(
            "int valid_socket(object caller, string op, mixed *info) {{ return op != \"{denied}\"; }}"
        );
        let f = Fixture::new(&policy, OWNER, RECIPIENT).await;
        f.established().await;
        assert!(
            f.call(&f.owner, "give", &["yes".into()])
                .await
                .unwrap_err()
                .to_string()
                .contains("permission denied")
        );
        assert_eq!(
            f.call(&f.owner, "owner", &[]).await.unwrap(),
            LpcRef::from(Arc::downgrade(&f.owner))
        );
        assert_eq!(
            f.call(&f.recipient, "received", &[]).await.unwrap(),
            LpcRef::from(0)
        );
    }
}

#[tokio::test]
async fn acquisition_rechecks_lifecycle_changed_by_the_master() {
    let f = Fixture::new(
        r#"
        int valid_socket(object caller, string op, mixed *info) {
            if (op == "acquire") info[1]->cancel(info[0]); return 1;
        }
    "#,
        OWNER,
        RECIPIENT,
    )
    .await;
    f.established().await;
    assert!(
        f.call(&f.owner, "give", &["yes".into()])
            .await
            .unwrap_err()
            .to_string()
            .contains("offered stream changed")
    );
    assert_eq!(
        f.call(&f.owner, "state", &[]).await.unwrap().as_str(),
        Some("open")
    );
    assert_eq!(
        f.call(&f.owner, "owner", &[]).await.unwrap(),
        LpcRef::from(Arc::downgrade(&f.owner))
    );
}

#[tokio::test]
async fn acquisition_rechecks_owner_retirement_by_the_master() {
    let fixture = Fixture::new(
        r#"
        int valid_socket(object caller, string op, mixed *info) {
            if (op == "acquire") destruct(info[1]);
            return 1;
        }
        "#,
        OWNER,
        RECIPIENT,
    )
    .await;
    fixture.established().await;
    assert!(
        fixture
            .call(&fixture.owner, "give", &["yes".into()])
            .await
            .unwrap_err()
            .to_string()
            .contains("invalid offer context")
    );
    assert_eq!(
        fixture.call(&fixture.owner, "owner", &[]).await.unwrap(),
        LpcRef::from(Arc::downgrade(&fixture.owner))
    );
}

#[tokio::test]
async fn offers_reject_connecting_streams_listeners_and_wrong_receivers() {
    let owner = format!(
        r#"{OWNER}
        int listen() {{ return handle = socket_listen("127.0.0.1", 0, event); }}
        int wrong() {{ return socket_release(handle, find_object("/recipient"), offered); }}
    "#
    );
    let f = Fixture::new(ALLOW, &owner, RECIPIENT).await;
    f.call(&f.owner, "open", &[]).await.unwrap();
    assert!(
        f.call(&f.owner, "give", &["yes".into()])
            .await
            .unwrap_err()
            .to_string()
            .contains("established")
    );
    f.call(&f.owner, "listen", &[]).await.unwrap();
    assert!(
        f.call(&f.owner, "give", &["yes".into()])
            .await
            .unwrap_err()
            .to_string()
            .contains("established")
    );
    f.established().await;
    assert!(
        f.call(&f.owner, "wrong", &[])
            .await
            .unwrap_err()
            .to_string()
            .contains("resolve to the recipient")
    );
}

#[tokio::test]
async fn conflicting_offer_attempt_does_not_publish_handover_and_fresh_attempt_can_accept() {
    let f = Fixture::new(ALLOW, OWNER, RECIPIENT).await;
    let h = f.established().await;
    let mut old_live = start_txn(&f.vm.global_state.committer_tx).await.unwrap();
    let old = TxnHandle::new(Transaction::new(old_live.inner.clone()));
    let mut template = TaskTemplate::from(f.vm.global_state.clone());
    template.txn = old.clone();
    let result = apply_function_by_name("give", &["yes".into()], f.owner.clone(), template, None)
        .await
        .unwrap()
        .unwrap();
    assert_eq!(result, LpcRef::from(1));
    assert_eq!(
        f.call(&f.owner, "owner", &[]).await.unwrap(),
        LpcRef::from(Arc::downgrade(&f.owner))
    );

    let mut newer_live = start_txn(&f.vm.global_state.committer_tx).await.unwrap();
    let mut context = TaskContext::new(f.vm.global_state.clone(), f.owner.clone(), None);
    context.txn = TxnHandle::new(Transaction::new(newer_live.inner.clone()));
    let access = SocketAccess::new(&context);
    let pointer = Arc::new(
        access
            .lease(h)
            .unwrap()
            .unwrap()
            .callback()
            .as_ref()
            .clone(),
    );
    access.transfer(h, &f.owner, &f.owner, pointer).unwrap();
    newer_live.disarm();
    commit_changeset(
        &f.vm.global_state.committer_tx,
        context.txn.with(Transaction::take_changeset),
    )
    .await
    .unwrap()
    .unwrap();
    old_live.disarm();
    assert!(
        commit_changeset(
            &f.vm.global_state.committer_tx,
            old.with(Transaction::take_changeset)
        )
        .await
        .unwrap()
        .is_err()
    );
    assert_eq!(
        f.call(&f.owner, "owner", &[]).await.unwrap(),
        LpcRef::from(Arc::downgrade(&f.owner))
    );
    assert_eq!(
        f.call(&f.recipient, "received", &[]).await.unwrap(),
        LpcRef::from(0)
    );
    assert_eq!(
        f.call(&f.owner, "give", &["yes".into()]).await.unwrap(),
        LpcRef::from(1)
    );
    assert_eq!(
        f.call(&f.owner, "owner", &[]).await.unwrap(),
        LpcRef::from(Arc::downgrade(&f.recipient))
    );
}

#[tokio::test]
async fn public_efuns_offer_binary_accepted_stream_and_respond_after_eof() {
    let mut f = Fixture::new(
        ALLOW,
        r#"
        int listener; int port; int handed;
        void event(int h, string event, mixed data) {
            if (event == "ready") port = data["local"][1];
            if (event == "accept") {
                object recipient = find_object("/recipient");
                handed = socket_release(data, recipient, recipient->offer());
            }
        }
        int start() { return listener = socket_listen("127.0.0.1", 0, event); }
        int port() { return port; }
        int handed() { return handed; }
        void cancel() { socket_close(listener); }
    "#,
        r#"
        bytes received = to_bytes("", "UTF-8"); int context_ok; int ended;
        void event(int h, string event, mixed data) {
            context_ok = origin() == "driver" && this_player() == nil && this_interactive() == nil;
            if (event == "read") { received += data; socket_write(h, data); }
            if (event == "eof") { socket_write(h, to_bytes("!", "UTF-8")); socket_close(h); }
            if (event == "closed") ended = socket_info(h)["state"] == "closed";
        }
        void accept(int h, object previous) { socket_acquire(h, event); }
        function offer() { return accept; }
        int ended() { return ended; }
        int context_ok() { return context_ok; }
        bytes received() { return received; }
    "#,
    )
    .await;
    f.vm.start_sockets().unwrap();
    f.vm.start_sockets().unwrap();
    f.call(&f.owner, "start", &[]).await.unwrap();
    let port = timeout(Duration::from_secs(3), async {
        loop {
            let LpcRef::Int(n) = f.call(&f.owner, "port", &[]).await.unwrap() else {
                panic!("port")
            };
            if n.0 > 0 {
                break n.0 as u16;
            }
            tokio::time::sleep(Duration::from_millis(5)).await;
        }
    })
    .await
    .unwrap();
    let mut peer = TcpStream::connect(("127.0.0.1", port)).await.unwrap();
    timeout(Duration::from_secs(3), async {
        while f.call(&f.owner, "handed", &[]).await.unwrap() != LpcRef::from(1) {
            tokio::time::sleep(Duration::from_millis(5)).await;
        }
    })
    .await
    .unwrap();
    f.call(&f.owner, "cancel", &[]).await.unwrap();
    peer.write_all(b"A\0").await.unwrap();
    peer.write_all(b"B").await.unwrap();
    let mut bytes = [0; 3];
    timeout(Duration::from_secs(3), peer.read_exact(&mut bytes))
        .await
        .unwrap()
        .unwrap();
    assert_eq!(&bytes, b"A\0B");
    peer.shutdown().await.unwrap();
    let mut end = Vec::new();
    timeout(Duration::from_secs(3), peer.read_to_end(&mut end))
        .await
        .unwrap()
        .unwrap();
    assert_eq!(end, b"!");
    timeout(Duration::from_secs(3), async {
        while f.call(&f.recipient, "ended", &[]).await.unwrap() != LpcRef::from(1) {
            tokio::time::sleep(Duration::from_millis(5)).await;
        }
    })
    .await
    .unwrap();
    assert_eq!(
        f.call(&f.recipient, "context_ok", &[]).await.unwrap(),
        LpcRef::from(1)
    );
    assert_eq!(
        f.call(&f.recipient, "received", &[])
            .await
            .unwrap()
            .as_bytes(),
        Some(b"A\0B".as_slice())
    );
    f.vm.shutdown().await.unwrap();
}

#[tokio::test]
async fn run_starts_public_connect_and_shutdown_closes_it_before_the_master_hook() {
    let peer_listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
    let port = peer_listener.local_addr().unwrap().port();
    let policy = format!(
        r#"{ALLOW}
        int valid_shutdown(object caller, string program) {{ return 1; }}
        int valid_write(string path, string func, object caller, string program) {{ return 1; }}
        int blocked;
        void shutdown() {{
            object owner = find_object("/owner");
            blocked = stringp(catch(owner->write()));
            write_file("/stopped", blocked ? "1" : "0");
        }}
    "#
    );
    let mut f = Fixture::new(
        &policy,
        r#"
        int handle; int ready; int reads;
        void event(int h, string name, mixed data) {
            if (name == "ready") { ready = 1; socket_write(h, to_bytes(({ 65, 0, 66 }))); }
            if (name == "read") reads += sizeof(data);
        }
        int open(int port) { return handle = socket_connect("127.0.0.1", port, event); }
        void write() { socket_write(handle, to_bytes("late", "UTF-8")); }
        int reads() { return reads; }
        void stop() { shutdown(0); }
    "#,
        "",
    )
    .await;
    f.call(&f.owner, "open", &[i64::from(port).into()])
        .await
        .unwrap();
    let state = f.vm.global_state.clone();
    let owner = f.owner.clone();
    let evidence = f._root.join("stopped");
    let running = tokio::spawn(async move { f.vm.run().await });
    let (mut peer, _) = timeout(Duration::from_secs(3), peer_listener.accept())
        .await
        .unwrap()
        .unwrap();
    let mut greeting = [0; 3];
    timeout(Duration::from_secs(3), peer.read_exact(&mut greeting))
        .await
        .unwrap()
        .unwrap();
    assert_eq!(&greeting, b"A\0B");
    peer.write_all(b"reply").await.unwrap();
    timeout(Duration::from_secs(3), async {
        loop {
            let result = apply_function_by_name(
                "reads",
                &[],
                owner.clone(),
                TaskTemplate::from(state.clone()),
                None,
            )
            .await
            .unwrap()
            .unwrap();
            if result == LpcRef::from(5) {
                break;
            }
            tokio::time::sleep(Duration::from_millis(5)).await;
        }
    })
    .await
    .unwrap();
    apply_function_by_name("stop", &[], owner, TaskTemplate::from(state.clone()), None)
        .await
        .unwrap()
        .unwrap();
    assert_eq!(
        timeout(Duration::from_secs(3), running)
            .await
            .unwrap()
            .unwrap()
            .unwrap(),
        0
    );
    let mut ending = Vec::new();
    timeout(Duration::from_secs(3), peer.read_to_end(&mut ending))
        .await
        .unwrap()
        .unwrap();
    assert!(ending.is_empty());
    assert_eq!(std::fs::read_to_string(evidence).unwrap(), "1");
}
