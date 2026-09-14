use std::sync::Arc;

use crate::{
    interpreter::{
        CommittedReader,
        process::Process,
        stm::{Conflict, LiveSnapshot, Transaction, TxnHandle, commit_changeset, start_txn},
        task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
        vm::Vm,
    },
    test_support::test_config,
};

struct Pending {
    live: LiveSnapshot,
    template: TaskTemplate,
}

impl Pending {
    async fn new(vm: &Vm) -> Self {
        let live = start_txn(&vm.global_state.committer_tx).await.unwrap();
        let mut template = TaskTemplate::from(vm.global_state.clone());
        template.txn = TxnHandle::new(Transaction::new(live.inner.clone()));
        Self { live, template }
    }

    async fn call(&self, process: &Arc<Process>, name: &str) {
        call(&self.template, process, name).await;
    }

    async fn commit(mut self, vm: &Vm) -> Result<(), Conflict> {
        self.live.disarm();
        commit_changeset(
            &vm.global_state.committer_tx,
            self.template.txn.with(|txn| txn.take_changeset()),
        )
        .await
        .unwrap()
    }
}

async fn call(template: &TaskTemplate, process: &Arc<Process>, name: &str) {
    apply_function_by_name(name, &[], process.clone(), template.clone(), None)
        .await
        .unwrap()
        .unwrap();
}

async fn object(vm: &Vm, path: &str, code: &str) -> Arc<Process> {
    vm.initialize_process_from_code(path, code)
        .await
        .unwrap()
        .context
        .process
}

fn assert_living_index(vm: &Vm, room: &Arc<Process>) {
    let inventory = vm.global_state.committed_inventory(room);
    let expected: Vec<_> = inventory
        .iter()
        .filter(|object| vm.global_state.commands_enabled(object))
        .collect();
    let actual = vm
        .global_state
        .committed_array(room.position.livings.id)
        .unwrap_or_default();
    assert_eq!(actual.len(), expected.len());
    for object in expected {
        assert_eq!(actual.iter().filter(|value| {
            matches!(value, crate::interpreter::lpc_ref::LpcRef::Object(weak) if std::ptr::eq(weak.as_ptr(), Arc::as_ptr(object)))
        }).count(), 1);
    }
}

#[tokio::test]
async fn concurrent_moves_cannot_form_a_containment_cycle() {
    let vm = Vm::new(test_config());
    let first = object(
        &vm,
        "/first.c",
        r#"void enter() { move_object("/second"); }"#,
    )
    .await;
    let second = object(
        &vm,
        "/second.c",
        r#"void enter() { move_object("/first"); }"#,
    )
    .await;
    let a = Pending::new(&vm).await;
    let b = Pending::new(&vm).await;
    a.call(&first, "enter").await;
    b.call(&second, "enter").await;
    a.commit(&vm).await.unwrap();
    b.commit(&vm)
        .await
        .expect_err("the destination's ancestor chain changed");
    let error = apply_function_by_name(
        "enter",
        &[],
        second.clone(),
        TaskTemplate::from(vm.global_state.clone()),
        None,
    )
    .await
    .unwrap()
    .expect_err("the new ancestor chain contains the mover");
    assert!(error.to_string().contains("would contain itself"));
    assert_eq!(
        vm.global_state.committed_environment(&first),
        Some(second.clone())
    );
    assert_eq!(vm.global_state.committed_environment(&second), None);
}

#[tokio::test]
async fn nonliving_arrivals_and_departures_commute_with_rule_cleanup() {
    let vm = Vm::new(test_config());
    let source = object(&vm, "/source.c", "").await;
    let destination = object(&vm, "/destination.c", "").await;
    let watcher = r#"
        void create() { enable_commands(); move_object("ROOM"); }
    "#;
    let old_watcher = object(&vm, "/old_watcher.c", &watcher.replace("ROOM", "/source")).await;
    let new_watcher = object(
        &vm,
        "/new_watcher.c",
        &watcher.replace("ROOM", "/destination"),
    )
    .await;
    let body = r#"
        void create() { move_object("/source"); }
        void enter() { move_object("/destination"); }
        void init() { add_action("greet", "greet"); }
        int greet(string arg) { return 1; }
    "#;
    let first = object(&vm, "/first.c", body).await;
    let second = object(&vm, "/second.c", body).await;
    assert_eq!(vm.global_state.committed_rules(&old_watcher).len(), 2);
    let a = Pending::new(&vm).await;
    let b = Pending::new(&vm).await;
    a.call(&first, "enter").await;
    b.call(&second, "enter").await;
    a.commit(&vm).await.unwrap();
    b.commit(&vm)
        .await
        .expect("distinct nonliving movers need no inventory read");
    assert_eq!(vm.global_state.committed_inventory(&source).len(), 1);
    assert_eq!(vm.global_state.committed_inventory(&destination).len(), 3);
    assert!(vm.global_state.committed_rules(&old_watcher).is_empty());
    let rules = vm.global_state.committed_rules(&new_watcher);
    for mover in [first, second] {
        assert_eq!(
            vm.global_state.committed_environment(&mover),
            Some(destination.clone())
        );
        assert_eq!(
            rules
                .iter()
                .filter(|rule| rule
                    .owner()
                    .is_some_and(|owner| Arc::ptr_eq(&owner, &mover)))
                .count(),
            1
        );
    }
    assert_living_index(&vm, &source);
    assert_living_index(&vm, &destination);
}

#[tokio::test]
async fn simultaneous_living_arrivals_retry_to_meet_each_other() {
    let vm = Vm::new(test_config());
    let room = object(&vm, "/destination.c", "").await;
    let body = r#"
        void create() { enable_commands(); }
        void enter() { move_object("/destination"); }
        void init() { add_action("greet", "greet"); }
        int greet(string arg) { return 1; }
    "#;
    let first = object(&vm, "/first.c", body).await;
    let second = object(&vm, "/second.c", body).await;
    let a = Pending::new(&vm).await;
    let b = Pending::new(&vm).await;
    a.call(&first, "enter").await;
    b.call(&second, "enter").await;
    a.commit(&vm).await.unwrap();
    b.commit(&vm)
        .await
        .expect_err("the second arrival has not met the first");
    call(
        &TaskTemplate::from(vm.global_state.clone()),
        &second,
        "enter",
    )
    .await;
    for (actor, other) in [(&first, &second), (&second, &first)] {
        let rules = vm.global_state.committed_rules(actor);
        assert_eq!(rules.len(), 1);
        assert!(
            rules[0]
                .owner()
                .is_some_and(|owner| Arc::ptr_eq(&owner, other))
        );
        assert_eq!(
            vm.global_state.committed_environment(actor),
            Some(room.clone())
        );
    }
    assert_living_index(&vm, &room);
}

#[tokio::test]
async fn a_residents_departure_invalidates_an_arrivals_meetings() {
    for living in [false, true] {
        let vm = Vm::new(test_config());
        object(&vm, "/destination.c", "").await;
        object(&vm, "/elsewhere.c", "").await;
        let peer = object(
            &vm,
            "/peer.c",
            r#"
            void create() { enable_commands(); move_object("/destination"); }
            void leave() { move_object("/elsewhere"); }
        "#,
        )
        .await;
        let actor = object(
            &vm,
            "/actor.c",
            &format!(
                r#"
            void create() {{ {} }}
            void enter() {{ move_object("/destination"); }}
            void init() {{ add_action("greet", "greet"); }}
            int greet(string arg) {{ return 1; }}
        "#,
                if living { "enable_commands();" } else { "" }
            ),
        )
        .await;
        let arrival = Pending::new(&vm).await;
        let departure = Pending::new(&vm).await;
        arrival.call(&actor, "enter").await;
        departure.call(&peer, "leave").await;
        departure.commit(&vm).await.unwrap();
        arrival
            .commit(&vm)
            .await
            .expect_err("the observed membership changed");
        call(
            &TaskTemplate::from(vm.global_state.clone()),
            &actor,
            "enter",
        )
        .await;
        assert!(vm.global_state.committed_rules(&peer).is_empty());
    }
}

#[tokio::test]
async fn enabling_or_disabling_during_a_move_retries_with_the_current_living_index() {
    for disabling in [false, true] {
        for move_commits_first in [false, true] {
            let vm = Vm::new(test_config());
            let source = object(&vm, "/source.c", "").await;
            let destination = object(&vm, "/destination.c", "").await;
            let actor = object(
                &vm,
                "/actor.c",
                &format!(
                    r#"
                void create() {{ {} move_object("/source"); }}
                void enter() {{ move_object("/destination"); }}
                void toggle() {{ {} }}
            "#,
                    if disabling { "enable_commands();" } else { "" },
                    if disabling {
                        "disable_commands();"
                    } else {
                        "enable_commands();"
                    }
                ),
            )
            .await;
            let movement = Pending::new(&vm).await;
            let toggle = Pending::new(&vm).await;
            movement.call(&actor, "enter").await;
            toggle.call(&actor, "toggle").await;
            let retry = if move_commits_first {
                movement.commit(&vm).await.unwrap();
                toggle
                    .commit(&vm)
                    .await
                    .expect_err("the object's environment changed");
                "toggle"
            } else {
                toggle.commit(&vm).await.unwrap();
                movement
                    .commit(&vm)
                    .await
                    .expect_err("the object's living state changed");
                "enter"
            };
            call(&TaskTemplate::from(vm.global_state.clone()), &actor, retry).await;
            assert_eq!(vm.global_state.commands_enabled(&actor), !disabling);
            assert_eq!(
                vm.global_state.committed_environment(&actor),
                Some(destination.clone())
            );
            assert_living_index(&vm, &source);
            assert_living_index(&vm, &destination);
        }
    }
}
