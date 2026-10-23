use bytes::Bytes;
use lpc_rs_core::{null_mode::NullMode, register::RegisterVariant};
use lpc_rs_utils::config::ConfigBuilder;
use tokio::time::{Duration, timeout};

use super::*;
use crate::{
    interpreter::{
        CommittedReader,
        function_type::{function_address::FunctionAddress, function_ptr::FunctionPtrBuilder},
        stm::RetryStats,
        task::apply_function::apply_function_by_name,
        vm::{Vm, vm_op::VmOp},
    },
    test_support::{PERMISSIVE_MASTER, TempLib, lib_holding},
};

const RECORDER: &str = r#"
    string events = "";
    mixed payload;
    int count;
    private void event(int handle, string name, mixed data) {
        events += name + ":"; payload = data; count++;
    }
    void die() { destruct(this_object()); }
"#;

struct Fixture {
    vm: Vm,
    root: TempLib,
    owner: Arc<Process>,
    master: Arc<Process>,
}

impl Fixture {
    async fn new(source: &str) -> Self {
        Self::with_budget(source, 4000).await
    }
    async fn with_budget(source: &str, budget: u64) -> Self {
        let master = format!(
            r#"{PERMISSIVE_MASTER}
            int errors; string last_error; object last_object;
            int valid_recompile() {{ return 1; }}
            void error_handler(mapping info) {{ errors++; last_error = info["error"]; last_object = info["object"]; }}
        "#
        );
        let root = lib_holding(
            "socket-callbacks",
            &[("secure/master.c", &master), ("owner.c", source)],
        );
        let vm = Vm::new(
            ConfigBuilder::default()
                .lib_dir(root.to_str().unwrap())
                .null_mode(NullMode::Distinct)
                .max_execution_time(budget)
                .max_pending_socket_input(8)
                .max_pending_socket_accepts(2)
                .call_out_preserve_player(true)
                .build()
                .unwrap(),
        );
        let master = vm
            .initialize_process_from_code("/secure/master.c", &master)
            .await
            .unwrap()
            .context
            .process;
        let owner = vm
            .initialize_process_from_code("/owner.c", source)
            .await
            .unwrap_or_else(|e| panic!("{}", e.diagnostic_string()))
            .context
            .process;
        Self {
            vm,
            root,
            owner,
            master,
        }
    }
    fn gs(&self) -> &Arc<GlobalState> {
        &self.vm.global_state
    }
    async fn begin(&self) -> (TxnHandle, LiveSnapshot) {
        let live = start_txn(&self.gs().committer_tx).await.unwrap();
        (TxnHandle::new(Transaction::new(live.inner.clone())), live)
    }
    fn access<'a>(&'a self, txn: &TxnHandle) -> SocketAccess<'a> {
        SocketAccess {
            sockets: &self.gs().sockets,
            txn: txn.clone(),
        }
    }
    async fn commit(&self, txn: &TxnHandle, mut live: LiveSnapshot) {
        live.disarm();
        commit_changeset(
            &self.gs().committer_tx,
            txn.with(Transaction::take_changeset),
        )
        .await
        .unwrap()
        .unwrap();
    }
    fn global(&self, process: &Arc<Process>, name: &str) -> LpcRef {
        let RegisterVariant::Global(reg) = process.initial_program().global_variables[name]
            .location
            .unwrap()
        else {
            panic!("global")
        };
        self.gs().committed_global(process, reg.index())
    }
    async fn object(&self, name: &str, source: &str) -> Arc<Process> {
        self.vm
            .initialize_process_from_code(name, source)
            .await
            .unwrap_or_else(|e| panic!("{}", e.diagnostic_string()))
            .context
            .process
    }
    async fn pointer(&self, owner: &Arc<Process>, name: &str) -> Arc<FunctionPtr> {
        let (txn, _live) = self.begin().await;
        let function = owner.program(&txn).lookup_function(name).unwrap().clone();
        Arc::new(
            FunctionPtrBuilder::default()
                .owner(Arc::downgrade(owner))
                .address(FunctionAddress::local(owner, function, &txn))
                .build()
                .unwrap(),
        )
    }
    async fn connect(&self, pointer: Arc<FunctionPtr>) -> SocketHandle {
        let (txn, live) = self.begin().await;
        let handle = self
            .access(&txn)
            .connect(&self.owner, "localhost".into(), 9, pointer)
            .unwrap();
        self.commit(&txn, live).await;
        handle
    }
    async fn ready(&self, handle: SocketHandle) {
        let (txn, live) = self.begin().await;
        let remote = match self.access(&txn).info(handle).unwrap().target {
            SocketTarget::Listen(_) => None,
            SocketTarget::Connect { .. } => Some(endpoint(2000)),
        };
        self.access(&txn)
            .ready(handle, endpoint(1000), remote)
            .unwrap();
        self.commit(&txn, live).await;
    }
    async fn stream(&self) -> SocketHandle {
        let handle = self.connect(self.pointer(&self.owner, "event").await).await;
        self.ready(handle).await;
        self.dispatch(handle).await;
        handle
    }
    async fn read(&self, handle: SocketHandle, seq: u64, data: &'static [u8]) {
        let (txn, live) = self.begin().await;
        assert!(
            self.access(&txn)
                .read(handle, seq, Bytes::from_static(data))
                .unwrap()
        );
        self.commit(&txn, live).await;
    }
    async fn head(&self, handle: SocketHandle) -> Pending {
        pending(self.gs())
            .await
            .unwrap()
            .into_iter()
            .find(|e| e.handle == handle)
            .unwrap()
    }
    async fn dispatch(&self, handle: SocketHandle) {
        dispatch(self.gs(), self.head(handle).await).await.unwrap();
    }
    async fn info(&self, handle: SocketHandle) -> Option<super::super::SocketInfo> {
        let (txn, _live) = self.begin().await;
        self.access(&txn).info(handle)
    }
    async fn call(&self, process: &Arc<Process>, name: &str) -> LpcRef {
        apply_function_by_name(
            name,
            &[],
            process.clone(),
            TaskTemplate::from(self.gs().clone()),
            Some(4000),
        )
        .await
        .unwrap()
        .unwrap()
    }
    async fn queue_update(
        &mut self,
        source: &str,
    ) -> Arc<crate::interpreter::vm::object_update::UpdateRequest> {
        std::fs::write(self.root.join("owner.c"), source).unwrap();
        let admin = self
            .object(
                "/admin.c",
                r#"int request() { return request_object_recompile(find_object("/owner")); }"#,
            )
            .await;
        self.call(&admin, "request").await;
        let Some(VmOp::ObjectUpdate(request)) = self.vm.next_op() else {
            panic!("update")
        };
        request
    }
    async fn upgrade(&mut self, source: &str) {
        let request = self.queue_update(source).await;
        self.gs().run_object_update(request.clone()).await;
        let status = self.gs().updates.get(request.id).unwrap();
        assert_eq!(status.state, "succeeded", "{}", status.error);
    }
}

fn endpoint(port: u16) -> std::net::SocketAddr {
    ([127, 0, 0, 1], port).into()
}

fn invocation(f: &Fixture, event: Pending) -> Invocation<'_> {
    Invocation {
        gs: f.gs(),
        event,
        txn: None,
        lease: None,
        task: None,
        failure: None,
    }
}

enum Action {
    Write(Arc<Process>, String, LpcRef),
    Transfer(SocketHandle, Arc<Process>, Arc<Process>, Arc<FunctionPtr>),
    Destruct(Arc<Process>),
    Update(Arc<crate::interpreter::vm::object_update::UpdateRequest>),
    Close(SocketHandle, Arc<Process>),
}

struct Race<'a> {
    body: Invocation<'a>,
    action: Option<Action>,
}

#[async_trait::async_trait]
impl AttemptBody for Race<'_> {
    async fn begin_attempt(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
    ) -> Result<Option<LiveSnapshot>> {
        self.body.begin_attempt(tx).await
    }
    async fn commit_phase(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
        live: LiveSnapshot,
    ) -> Result<(std::result::Result<(), Conflict>, Vec<Effect>)> {
        if let Some(action) = self.action.take() {
            let mut other = start_txn(tx).await?;
            let txn = TxnHandle::new(Transaction::new(other.inner.clone()));
            match action {
                Action::Write(process, name, value) => txn.with(|t| {
                    let image = process.image_in(t);
                    let RegisterVariant::Global(reg) =
                        image.program.global_variables[&name].location.unwrap()
                    else {
                        panic!("global")
                    };
                    t.write(image.var_id(reg.index()), value);
                }),
                Action::Transfer(handle, from, to, callback) => SocketAccess {
                    sockets: &self.body.gs.sockets,
                    txn: txn.clone(),
                }
                .transfer(handle, &from, &to, callback)?,
                Action::Destruct(process) => {
                    let mut template = TaskTemplate::from(self.body.gs.clone());
                    template.txn = txn.clone();
                    apply_function_by_name("die", &[], process, template, Some(4000))
                        .await
                        .unwrap()?;
                }
                Action::Update(request) => {
                    self.body.gs.run_object_update(request.clone()).await;
                    let status = self.body.gs.updates.get(request.id).unwrap();
                    assert_eq!(status.state, "succeeded", "{}", status.error);
                }
                Action::Close(handle, owner) => SocketAccess {
                    sockets: &self.body.gs.sockets,
                    txn: txn.clone(),
                }
                .close(&owner, handle)?,
            }
            other.disarm();
            commit_changeset(tx, txn.with(Transaction::take_changeset))
                .await?
                .unwrap();
        }
        self.body.commit_phase(tx, live).await
    }
    async fn deliver(&mut self, effects: Vec<Effect>) -> Result<()> {
        self.body.deliver(effects).await
    }
}

async fn raced(f: &Fixture, event: Pending, action: Action) -> RetryStats {
    let mut race = Race {
        body: invocation(f, event),
        action: Some(action),
    };
    let (result, stats) = f.gs().attempt_runner.run(&mut race).await;
    result.unwrap();
    assert!(race.body.failure.is_none());
    assert_eq!(stats.attempts, 2);
    assert_eq!(stats.conflicts, 1);
    stats
}

#[tokio::test]
async fn callbacks_receive_ordered_raw_bytes_eof_and_drained() {
    let f = Fixture::new(RECORDER).await;
    let handle = f.stream().await;
    f.read(handle, 1, b"a\0b").await;
    let (txn, live) = f.begin().await;
    f.access(&txn).eof(handle, 2).unwrap();
    f.access(&txn)
        .write(&f.owner, handle, Bytes::from_static(b"response"))
        .unwrap();
    f.commit(&txn, live).await;
    let (txn, live) = f.begin().await;
    f.access(&txn).complete_write(handle, 1).unwrap();
    f.commit(&txn, live).await;
    f.dispatch(handle).await;
    assert_eq!(
        f.global(&f.owner, "payload"),
        LpcRef::from(Bytes::from_static(b"a\0b"))
    );
    f.dispatch(handle).await;
    assert_eq!(f.global(&f.owner, "payload"), LpcRef::Nil);
    assert_eq!(
        f.info(handle).await.unwrap().state,
        super::super::SocketState::Open
    );
    f.dispatch(handle).await;
    assert_eq!(f.global(&f.owner, "payload"), LpcRef::Nil);
    assert_eq!(
        f.global(&f.owner, "events"),
        LpcRef::from("ready:read:eof:drained:")
    );
    assert!(pending(f.gs()).await.unwrap().is_empty());
}

#[tokio::test]
async fn retry_reconstructs_payloads_and_commits_callback_and_ack_together() {
    let f = Fixture::new(
        r#"
        int conflict; int count; int port;
        void event(int h, string e, mapping data) {
            port = data["local"][1]; data["local"][1] = 99;
            count += 1 + conflict;
        }
    "#,
    )
    .await;
    let handle = f.connect(f.pointer(&f.owner, "event").await).await;
    f.ready(handle).await;
    let event = f.head(handle).await;
    raced(
        &f,
        event,
        Action::Write(f.owner.clone(), "conflict".into(), LpcRef::from(1)),
    )
    .await;
    assert_eq!(f.global(&f.owner, "count"), LpcRef::from(2));
    assert_eq!(f.global(&f.owner, "port"), LpcRef::from(1000));
    assert!(pending(f.gs()).await.unwrap().is_empty());
}

#[tokio::test]
async fn retries_use_the_new_owner_and_callback_for_the_same_read() {
    let f = Fixture::new(RECORDER).await;
    let handle = f.stream().await;
    f.read(handle, 1, b"same").await;
    let recipient = f.object("/recipient.c", RECORDER).await;
    let callback = f.pointer(&recipient, "event").await;
    raced(
        &f,
        f.head(handle).await,
        Action::Transfer(handle, f.owner.clone(), recipient.clone(), callback),
    )
    .await;
    assert_eq!(f.global(&f.owner, "events"), LpcRef::from("ready:"));
    assert_eq!(f.global(&recipient, "events"), LpcRef::from("read:"));
    assert_eq!(
        f.global(&recipient, "payload"),
        LpcRef::from(Bytes::from_static(b"same"))
    );
    let (txn, _live) = f.begin().await;
    assert_eq!(f.access(&txn).input_capacity(handle).unwrap(), Some(8));
}

#[tokio::test]
async fn an_error_from_a_stale_registration_is_retried_without_reporting_or_discard() {
    let f = Fixture::new(
        r#"
        int count;
        void event(int h, string e, mixed d) { count++; if (e == "read") throw("old failure"); }
    "#,
    )
    .await;
    let handle = f.stream().await;
    f.read(handle, 1, b"same").await;
    let recipient = f.object("/recipient.c", RECORDER).await;
    let callback = f.pointer(&recipient, "event").await;
    raced(
        &f,
        f.head(handle).await,
        Action::Transfer(handle, f.owner.clone(), recipient.clone(), callback),
    )
    .await;
    assert_eq!(f.global(&f.owner, "count"), LpcRef::from(1));
    assert_eq!(f.global(&f.master, "errors"), LpcRef::from(0));
    assert_eq!(
        f.global(&recipient, "payload"),
        LpcRef::from(Bytes::from_static(b"same"))
    );
}

#[tokio::test]
async fn owner_destruction_invalidates_a_running_callback_and_removes_its_event() {
    let f = Fixture::new(RECORDER).await;
    let handle = f.connect(f.pointer(&f.owner, "event").await).await;
    f.ready(handle).await;
    raced(&f, f.head(handle).await, Action::Destruct(f.owner.clone())).await;
    assert!(f.info(handle).await.is_none());
    assert!(pending(f.gs()).await.unwrap().is_empty());
}

#[tokio::test]
async fn runtime_error_rolls_back_writes_and_timers_and_releases_only_the_failed_head() {
    let f = Fixture::new(
        r#"
        int count; string data;
        void timer() {}
        void event(int h, string e, mixed d) {
            count++;
            if (e == "read") {
                if (to_text(d, "UTF-8") == "bad") { call_out(timer, 60); throw("bad read"); }
                data = to_text(d, "UTF-8");
            }
        }
    "#,
    )
    .await;
    let handle = f.stream().await;
    f.read(handle, 1, b"bad").await;
    f.read(handle, 2, b"good").await;
    f.dispatch(handle).await;
    assert_eq!(f.global(&f.master, "errors"), LpcRef::from(1));
    assert_eq!(f.global(&f.owner, "count"), LpcRef::from(1));
    assert_eq!(f.gs().with_call_outs(|calls| calls.len()), 0);
    let (txn, _live) = f.begin().await;
    assert_eq!(f.access(&txn).input_capacity(handle).unwrap(), Some(4));
    f.dispatch(handle).await;
    assert_eq!(f.global(&f.owner, "data"), LpcRef::from("good"));
    assert_eq!(f.global(&f.owner, "count"), LpcRef::from(2));
}

#[tokio::test]
async fn caught_callback_errors_commit_normally() {
    let f = Fixture::new(
        r#"
        int count;
        void event(int h, string e, mixed d) { count++; catch(throw("caught")); count++; }
    "#,
    )
    .await;
    f.stream().await;
    assert_eq!(f.global(&f.owner, "count"), LpcRef::from(2));
    assert_eq!(f.global(&f.master, "errors"), LpcRef::from(0));
}

#[tokio::test]
async fn callbacks_use_driver_origin_and_the_scheduling_owner_without_player_context() {
    let f = Fixture::new(RECORDER).await;
    let receiver = f
        .object(
            "/receiver.c",
            r#"
        string called_from; object previous; object player; object interactive; string command;
        void event(int h, string e, mixed d) {
            called_from = origin(); previous = previous_object();
            player = this_player(); interactive = this_interactive(); command = query_command();
            call_out((: 1 :), 60);
        }
    "#,
        )
        .await;
    let handle = f.connect(f.pointer(&receiver, "event").await).await;
    f.ready(handle).await;
    f.dispatch(handle).await;
    assert_eq!(f.global(&receiver, "called_from"), LpcRef::from("driver"));
    assert_eq!(
        f.global(&receiver, "previous"),
        LpcRef::from(Arc::downgrade(&f.owner))
    );
    assert_eq!(f.global(&receiver, "player"), LpcRef::Nil);
    assert_eq!(f.global(&receiver, "interactive"), LpcRef::Nil);
    assert_eq!(f.global(&receiver, "command"), LpcRef::Nil);
    f.gs()
        .with_call_outs(|calls| assert!(calls.queue().iter().all(|(_, c)| c.player.is_none())));
}

#[tokio::test]
async fn bound_private_functions_and_captured_closures_use_native_pointer_rules() {
    let f = Fixture::new(r#"
        int total; string called_from;
        private void event(int prefix, int h, string e, mixed d) { total += prefix; called_from = origin(); }
        function bound() { return &event(7); }
        function closure() { int captured = 11; return (: [int h, string e, mixed d] { total += captured; called_from = origin(); } :); }
    "#).await;
    for name in ["bound", "closure"] {
        let LpcRef::Function(pointer) = f.call(&f.owner, name).await else {
            panic!("pointer")
        };
        let handle = f.connect(pointer).await;
        f.ready(handle).await;
        f.gs().gc().await.unwrap().unwrap();
        f.dispatch(handle).await;
    }
    assert_eq!(f.global(&f.owner, "total"), LpcRef::from(18));
    assert_eq!(f.global(&f.owner, "called_from"), LpcRef::from("driver"));
}

#[tokio::test]
async fn incompatible_callback_arguments_are_reported_and_retire_the_registration() {
    let f = Fixture::new("int calls; void event(string h, string e, mixed d) { calls++; }").await;
    let handle = f.connect(f.pointer(&f.owner, "event").await).await;
    f.ready(handle).await;
    f.dispatch(handle).await;
    assert!(f.info(handle).await.is_none());
    assert_eq!(f.global(&f.owner, "calls"), LpcRef::from(0));
    assert_eq!(f.global(&f.master, "errors"), LpcRef::from(1));
}

#[tokio::test]
async fn missing_dynamic_callback_retires_instead_of_consuming_each_read() {
    let f = Fixture::new(RECORDER).await;
    let pointer = Arc::new(
        FunctionPtrBuilder::default()
            .owner(Arc::downgrade(&f.owner))
            .address(FunctionAddress::Dynamic(ustr::ustr("missing")))
            .partial_args(thin_vec::thin_vec![Some(LpcRef::from(Arc::downgrade(
                &f.owner
            )))])
            .build()
            .unwrap(),
    );
    let handle = f.connect(pointer).await;
    f.ready(handle).await;
    f.read(handle, 1, b"a").await;
    f.read(handle, 2, b"b").await;
    f.dispatch(handle).await;
    assert!(f.info(handle).await.is_none());
    assert_eq!(f.global(&f.master, "errors"), LpcRef::from(1));
    assert!(pending(f.gs()).await.unwrap().is_empty());
}

#[tokio::test]
async fn receiver_destruction_reports_dispatch_failure_and_retires_socket() {
    let f = Fixture::new(RECORDER).await;
    let receiver = f
        .object(
            "/receiver.c",
            "void event(int h, string e, mixed d) {} void die() { destruct(this_object()); }",
        )
        .await;
    let handle = f.connect(f.pointer(&receiver, "event").await).await;
    f.ready(handle).await;
    f.call(&receiver, "die").await;
    f.dispatch(handle).await;
    assert!(f.info(handle).await.is_none());
    assert_eq!(f.global(&f.master, "errors"), LpcRef::from(1));
}

#[tokio::test]
async fn terminal_callback_error_still_retires_metadata_and_quota() {
    let f = Fixture::new(
        "void event(int h, string e, mixed d) { if (e == \"closed\") throw(\"closed failure\"); }",
    )
    .await;
    let handle = f.stream().await;
    let (txn, live) = f.begin().await;
    f.access(&txn).closed(handle);
    f.commit(&txn, live).await;
    assert!(f.info(handle).await.is_some());
    f.dispatch(handle).await;
    assert!(f.info(handle).await.is_none());
    let (txn, _live) = f.begin().await;
    assert!(f.access(&txn).query(Some(&f.owner)).unwrap().is_empty());
}

#[tokio::test]
async fn failing_accept_closes_its_undelivered_child() {
    let f = Fixture::new(
        "void event(int h, string e, mixed d) { if (e == \"accept\") throw(\"accept failure\"); }",
    )
    .await;
    let (txn, live) = f.begin().await;
    let listener = f
        .access(&txn)
        .listen(&f.owner, endpoint(0), f.pointer(&f.owner, "event").await)
        .unwrap();
    f.commit(&txn, live).await;
    f.ready(listener).await;
    f.dispatch(listener).await;
    let (txn, live) = f.begin().await;
    let super::super::SocketAdmission::Admitted(permit) =
        f.access(&txn).reserve_accept(listener).unwrap()
    else {
        panic!("permit")
    };
    f.commit(&txn, live).await;
    let (txn, live) = f.begin().await;
    f.access(&txn)
        .accepted(permit, endpoint(1000), endpoint(2000))
        .unwrap();
    f.commit(&txn, live).await;
    assert!(f.info(permit.child).await.is_some());
    f.dispatch(listener).await;
    assert!(f.info(permit.child).await.is_none());
    assert!(f.info(listener).await.is_some());
    assert_eq!(f.global(&f.master, "errors"), LpcRef::from(1));
}

#[tokio::test]
async fn failed_event_cleanup_cannot_consume_a_new_registration() {
    let f = Fixture::new(RECORDER).await;
    let handle = f.stream().await;
    f.read(handle, 1, b"a").await;
    let (txn, _live) = f.begin().await;
    let lease = f.access(&txn).lease(handle).unwrap().unwrap();
    let mut cleanup = Cleanup {
        gs: f.gs(),
        event: f.head(handle).await,
        owner: Arc::downgrade(lease.owner()),
        callback: Arc::downgrade(lease.callback()),
        retire: true,
        txn: None,
    };
    let recipient = f.object("/recipient.c", RECORDER).await;
    let (txn, live) = f.begin().await;
    f.access(&txn)
        .transfer(
            handle,
            &f.owner,
            &recipient,
            f.pointer(&recipient, "event").await,
        )
        .unwrap();
    f.commit(&txn, live).await;
    f.gs().attempt_runner.run(&mut cleanup).await.0.unwrap();
    assert!(f.info(handle).await.is_some());
    f.dispatch(handle).await;
    assert_eq!(
        f.global(&recipient, "payload"),
        LpcRef::from(Bytes::from_static(b"a"))
    );
}

#[tokio::test]
async fn named_callbacks_resolve_updated_code_and_missing_functions_retire() {
    let mut f =
        Fixture::new("int total; void event(int h, string e, mixed d) { total += 1; }").await;
    let handle = f.stream().await;
    f.read(handle, 1, b"a").await;
    f.upgrade("int total; void event(int h, string e, mixed d) { total += 10; }")
        .await;
    f.dispatch(handle).await;
    assert_eq!(f.global(&f.owner, "total"), LpcRef::from(11));
    f.read(handle, 2, b"b").await;
    f.upgrade("int total;").await;
    f.dispatch(handle).await;
    assert!(f.info(handle).await.is_none());
    assert_eq!(f.global(&f.master, "errors"), LpcRef::from(1));
}

#[tokio::test]
async fn anonymous_callbacks_retain_code_and_captures_across_update_and_gc() {
    let mut f = Fixture::new(r#"
        int total; function saved;
        void create() { int value = 7; saved = (: [int h, string e, mixed d] { total += value; } :); }
        function registration() { return saved; }
    "#).await;
    let LpcRef::Function(pointer) = f.call(&f.owner, "registration").await else {
        panic!("pointer")
    };
    let handle = f.connect(pointer).await;
    f.ready(handle).await;
    f.upgrade("int total; function saved;").await;
    f.gs().gc().await.unwrap().unwrap();
    f.dispatch(handle).await;
    assert_eq!(f.global(&f.owner, "total"), LpcRef::from(7));
}

#[tokio::test]
async fn dispatcher_serializes_one_handle_and_finds_events_without_raw_observations() {
    let f = Fixture::new(RECORDER).await;
    let handle = f.connect(f.pointer(&f.owner, "event").await).await;
    f.ready(handle).await;
    f.read(handle, 1, b"a").await;
    f.read(handle, 2, b"b").await;
    let runtime = SocketRuntime::spawn(f.gs(), f.gs().attempt_runner.commit_watch().unwrap(), None);
    timeout(Duration::from_secs(3), async {
        while f.global(&f.owner, "count") != LpcRef::from(3) {
            tokio::task::yield_now().await;
        }
    })
    .await
    .unwrap();
    runtime.shutdown().await.unwrap();
    assert_eq!(
        f.global(&f.owner, "events"),
        LpcRef::from("ready:read:read:")
    );
    assert_eq!(
        f.global(&f.owner, "payload"),
        LpcRef::from(Bytes::from_static(b"b"))
    );
    assert!(pending(f.gs()).await.unwrap().is_empty());
}

#[tokio::test]
async fn another_socket_progresses_while_a_callback_runs_and_shutdown_aborts_its_attempt() {
    let f =
        Fixture::new("int count; void event(int h, string e, mixed d) { count++; while (1) {} }")
            .await;
    let blocked = f.connect(f.pointer(&f.owner, "event").await).await;
    f.ready(blocked).await;
    let receiver = f.object("/receiver.c", RECORDER).await;
    let other = f.connect(f.pointer(&receiver, "event").await).await;
    f.ready(other).await;
    let runtime = SocketRuntime::spawn(f.gs(), f.gs().attempt_runner.commit_watch().unwrap(), None);
    timeout(Duration::from_secs(2), async {
        while f.global(&receiver, "count") != LpcRef::from(1) {
            tokio::task::yield_now().await;
        }
    })
    .await
    .unwrap();
    runtime.shutdown().await.unwrap();
    assert_eq!(f.global(&f.owner, "count"), LpcRef::from(0));
    assert_eq!(pending(f.gs()).await.unwrap().len(), 1);
    f.gs().gc().await.unwrap().unwrap();
}

#[tokio::test]
async fn efun_callbacks_use_the_native_entry_frame() {
    let f = Fixture::new(RECORDER).await;
    let pointer = Arc::new(
        FunctionPtrBuilder::default()
            .owner(Arc::downgrade(&f.owner))
            .address(FunctionAddress::Efun(ustr::ustr("dump")))
            .build()
            .unwrap(),
    );
    let handle = f.connect(pointer).await;
    f.ready(handle).await;
    f.dispatch(handle).await;
    assert!(pending(f.gs()).await.unwrap().is_empty());
    assert_eq!(f.global(&f.master, "errors"), LpcRef::from(0));
}

#[tokio::test]
async fn successful_accept_activates_its_child_after_the_lpc_callback_commits() {
    let f = Fixture::new(
        r#"
        int child; bytes data;
        void event(int h, string e, mixed d) {
            if (e == "accept") child = d;
            if (e == "read") data = d;
        }
    "#,
    )
    .await;
    let (txn, live) = f.begin().await;
    let listener = f
        .access(&txn)
        .listen(&f.owner, endpoint(0), f.pointer(&f.owner, "event").await)
        .unwrap();
    f.commit(&txn, live).await;
    f.ready(listener).await;
    f.dispatch(listener).await;
    let (txn, live) = f.begin().await;
    let super::super::SocketAdmission::Admitted(permit) =
        f.access(&txn).reserve_accept(listener).unwrap()
    else {
        panic!("permit")
    };
    f.commit(&txn, live).await;
    let (txn, live) = f.begin().await;
    f.access(&txn)
        .accepted(permit, endpoint(1000), endpoint(2000))
        .unwrap();
    f.commit(&txn, live).await;
    let (txn, _live) = f.begin().await;
    assert_eq!(f.access(&txn).input_capacity(permit.child).unwrap(), None);
    f.dispatch(listener).await;
    assert_eq!(
        f.global(&f.owner, "child"),
        LpcRef::from(permit.child.get())
    );
    let (txn, _live) = f.begin().await;
    assert_eq!(
        f.access(&txn).input_capacity(permit.child).unwrap(),
        Some(8)
    );
    f.read(permit.child, 1, b"child").await;
    f.dispatch(permit.child).await;
    assert_eq!(
        f.global(&f.owner, "data"),
        LpcRef::from(Bytes::from_static(b"child"))
    );
}

#[tokio::test]
async fn tcp_runtime_delivers_actual_bytes_eof_and_terminal_metadata_to_lpc() {
    use tokio::{io::AsyncWriteExt, net::TcpListener};
    let f = Fixture::new(r#"
        int ready; int done; bytes data = to_bytes("", "UTF-8"); int closed_ok;
        void event(int h, string e, mixed d) {
            if (e == "ready") ready = d["kind"] == "stream" && sizeof(d["local"]) == 2 && sizeof(d["remote"]) == 2;
            if (e == "read") data += d;
            if (e == "eof") { if (nilp(d)) done = 1; }
            if (e == "closed") {
                closed_ok = d["reason"] == "local" && nilp(d["operation"]) && nilp(d["error"])
                    && sizeof(d["local"]) == 2 && sizeof(d["remote"]) == 2;
                done = 2;
            }
        }
    "#).await;
    let server = TcpListener::bind(endpoint(0)).await.unwrap();
    let runtime = SocketRuntime::start(f.gs()).unwrap();
    assert!(SocketRuntime::start(f.gs()).is_err());
    let callback = f.pointer(&f.owner, "event").await;
    let (txn, live) = f.begin().await;
    let handle = f
        .access(&txn)
        .connect(
            &f.owner,
            "127.0.0.1".into(),
            server.local_addr().unwrap().port(),
            callback,
        )
        .unwrap();
    f.commit(&txn, live).await;
    let (mut peer, _) = timeout(Duration::from_secs(3), server.accept())
        .await
        .unwrap()
        .unwrap();
    peer.write_all(b"a\0bc").await.unwrap();
    peer.shutdown().await.unwrap();
    timeout(Duration::from_secs(3), async {
        while f.global(&f.owner, "done") != LpcRef::from(1) {
            tokio::task::yield_now().await;
        }
    })
    .await
    .unwrap();
    assert_eq!(f.global(&f.owner, "ready"), LpcRef::from(1));
    assert_eq!(
        f.global(&f.owner, "data"),
        LpcRef::from(Bytes::from_static(b"a\0bc"))
    );
    let (txn, live) = f.begin().await;
    f.access(&txn).close(&f.owner, handle).unwrap();
    f.commit(&txn, live).await;
    timeout(Duration::from_secs(3), async {
        while f.global(&f.owner, "done") != LpcRef::from(2) {
            tokio::task::yield_now().await;
        }
    })
    .await
    .unwrap();
    assert_eq!(f.global(&f.owner, "closed_ok"), LpcRef::from(1));
    assert!(f.info(handle).await.is_none());
    runtime.shutdown().await.unwrap();
}

#[tokio::test]
async fn a_recompile_during_dispatch_retries_the_same_event_against_new_code() {
    let mut f =
        Fixture::new("int total; void event(int h, string e, mixed d) { total += 1; }").await;
    let handle = f.stream().await;
    f.read(handle, 1, b"a").await;
    let request = f
        .queue_update("int total; void event(int h, string e, mixed d) { total += 10; }")
        .await;
    raced(&f, f.head(handle).await, Action::Update(request)).await;
    assert_eq!(f.global(&f.owner, "total"), LpcRef::from(11));
    assert!(pending(f.gs()).await.unwrap().is_empty());
}

#[tokio::test]
async fn evaluation_timeout_reports_and_discards_the_event_without_callback_writes() {
    let f = Fixture::with_budget(RECORDER, 500).await;
    let receiver = f.object("/receiver.c", "int count; void event(int h, string e, mixed d) { count++; if (e == \"ready\") while (1) {} }").await;
    let handle = f.connect(f.pointer(&receiver, "event").await).await;
    f.ready(handle).await;
    f.dispatch(handle).await;
    assert_eq!(f.global(&receiver, "count"), LpcRef::from(0));
    assert_eq!(f.global(&f.master, "errors"), LpcRef::from(1));
    assert_eq!(
        f.global(&f.master, "last_object"),
        LpcRef::from(Arc::downgrade(&receiver))
    );
    assert!(
        f.global(&f.master, "last_error")
            .to_string()
            .contains("evaluation limit")
    );
    assert!(pending(f.gs()).await.unwrap().is_empty());
    f.read(handle, 1, b"a").await;
    f.dispatch(handle).await;
    assert_eq!(f.global(&receiver, "count"), LpcRef::from(1));
    f.gs().gc().await.unwrap().unwrap();
}

#[tokio::test]
async fn accept_closes_child_when_its_callback_destroys_its_receiver() {
    for pointer_owned_by_receiver in [true, false] {
        let f = Fixture::new(RECORDER).await;
        let receiver = f
            .object(
                "/receiver.c",
                r#"
        void event(int h, string e, mixed d) {
            if (e == "accept") destruct(this_object());
        }
    "#,
            )
            .await;
        let (txn, live) = f.begin().await;
        let mut callback = f.pointer(&receiver, "event").await;
        if !pointer_owned_by_receiver {
            callback = Arc::new(
                FunctionPtrBuilder::default()
                    .owner(Arc::downgrade(&f.owner))
                    .address(callback.address.clone())
                    .build()
                    .unwrap(),
            );
        }
        let listener = f
            .access(&txn)
            .listen(&f.owner, endpoint(0), callback)
            .unwrap();
        f.commit(&txn, live).await;
        f.ready(listener).await;
        f.dispatch(listener).await;
        let (txn, live) = f.begin().await;
        let super::super::SocketAdmission::Admitted(permit) =
            f.access(&txn).reserve_accept(listener).unwrap()
        else {
            panic!("permit");
        };
        f.commit(&txn, live).await;
        let (txn, live) = f.begin().await;
        f.access(&txn)
            .accepted(permit, endpoint(1000), endpoint(2000))
            .unwrap();
        f.commit(&txn, live).await;
        f.dispatch(listener).await;
        let (txn, _live) = f.begin().await;
        assert!(!receiver.is_live(&txn));
        assert!(f.owner.is_live(&txn));
        let capacity = f.access(&txn).input_capacity(permit.child).unwrap();
        assert_eq!(capacity, None);
        assert!(pending(f.gs()).await.unwrap().is_empty());
        assert!(
            f.info(permit.child).await.is_none(),
            "accepted child retained and activated a dead callback"
        );
    }
}

#[tokio::test]
async fn close_suppresses_a_queued_drained_callback_but_delivers_closed() {
    let f = Fixture::new(RECORDER).await;
    let handle = f.stream().await;
    let (txn, live) = f.begin().await;
    assert!(
        f.access(&txn)
            .write(&f.owner, handle, Bytes::from_static(b"a"))
            .unwrap()
    );
    f.access(&txn).complete_write(handle, 1).unwrap();
    f.commit(&txn, live).await;
    let (txn, live) = f.begin().await;
    f.access(&txn).close(&f.owner, handle).unwrap();
    f.commit(&txn, live).await;
    assert!(pending(f.gs()).await.unwrap().is_empty());
    let (txn, live) = f.begin().await;
    f.access(&txn).closed(handle);
    f.commit(&txn, live).await;
    f.dispatch(handle).await;
    assert_eq!(f.global(&f.owner, "events"), LpcRef::from("ready:closed:"));
}

#[tokio::test]
async fn close_invalidates_a_running_drained_callback_and_rolls_back_its_writes() {
    let f = Fixture::new(RECORDER).await;
    let handle = f.stream().await;
    let (txn, live) = f.begin().await;
    assert!(
        f.access(&txn)
            .write(&f.owner, handle, Bytes::from_static(b"a"))
            .unwrap()
    );
    f.access(&txn).complete_write(handle, 1).unwrap();
    f.commit(&txn, live).await;
    let stats = raced(
        &f,
        f.head(handle).await,
        Action::Close(handle, f.owner.clone()),
    )
    .await;
    assert!(stats.conflicts > 0);
    assert_eq!(f.global(&f.owner, "events"), LpcRef::from("ready:"));
    assert_eq!(f.global(&f.owner, "count"), LpcRef::from(1));
    assert!(pending(f.gs()).await.unwrap().is_empty());
}
