use std::sync::Arc;

use if_chain::if_chain;
use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::efun_context::EfunContext,
    lpc_ref::{LpcRef, NULL},
    stm::Effect,
};

/// What a connection hears when another takes over its body.
pub(crate) const DISPLACED: &str =
    "You are being disconnected because someone else logged in as you.";

/// `exec`, an efun for moving a connection into an object.
///
/// The binding is transactional: the connection cells of both bodies are
/// written into this transaction, so the rest of the task (and efuns it
/// calls in the same attempt, e.g. `interactive()`) sees the handover before
/// commit. The socket-level handover — the connection's back-reference to
/// the new body (`Effect::Exec`) and the close of the connection it
/// displaced (`Effect::Disconnect`) — is deferred to after this task
/// commits, so a rejected attempt never touches the physical connection.
pub async fn exec<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    if_chain! {
        let new_ref = context.resolve_local_register(1 as RegisterSize);
        if let LpcRef::Object(new_ob) = new_ref;
        if let Some(new_ob) = new_ob.upgrade();
        let old_ref = context.resolve_local_register(2 as RegisterSize);
        if let LpcRef::Object(old_ob) = old_ref;
        if let Some(old_ob) = old_ob.upgrade();
        then {
            // One cell: the writes below would land `Some` then `None`
            // and the displaced holder would be the connection itself.
            if Arc::ptr_eq(&new_ob, &old_ob) {
                return Err(context.runtime_error("exec: `new` and `old` are the same object"));
            }

            let txn = context.txn();

            // The connection being moved, from the old body's cell.
            // Changeset-first, so a prior uncommitted `exec` in this same
            // task is seen.
            let connection = txn.with(|t| t.read_connection(old_ob.connection.id));
            let Some(connection) = connection else {
                context.return_efun_result(NULL);
                return Ok(());
            };

            // The connection the new body currently holds (if any);
            // the handover displaces it.
            let previous = txn.with(|t| t.read_connection(new_ob.connection.id));

            // Bind the new body, unbind the old one. Both writes land
            // in the changeset and commit with this task.
            txn.with(|t| t.write_connection(new_ob.connection.id, Some(connection.clone())));
            txn.with(|t| t.write_connection(old_ob.connection.id, None));

            context.record_effect(Effect::Exec {
                new_process: new_ob.clone(),
                connection,
            });
            if let Some(previous) = previous {
                context.record_effect(Effect::Disconnect {
                    connection: previous,
                    message: Some(DISPLACED.to_owned()),
                });
            }

            context.return_efun_result(LpcRef::from(1));
            Ok(())
        }
        else {
            context.return_efun_result(NULL);
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{net::ToSocketAddrs, sync::Arc};

    use indoc::indoc;

    use crate::{
        interpreter::{
            CommittedReader, lpc_int::LpcInt, lpc_ref::LpcRef, task::Task,
            task::task_template::TaskTemplate, vm::Vm,
        },
        telnet::{connection::Connection, ops::ConnectionOp},
        test_support::{connect, test_config},
    };

    #[tokio::test]
    async fn the_displaced_holder_hears_why_and_is_closed() {
        let vm = Vm::new(test_config());
        let a = vm.create_process_from_code("/a.c", "").await.unwrap();
        let b = vm.create_process_from_code("/b.c", "").await.unwrap();
        let mut on_a = connect(&vm, &a).await;
        let mut on_b = connect(&vm, &b).await;
        let main = indoc! { r#"
            void create() { exec(find_object("/b"), find_object("/a")); }
        "# };
        vm.initialize_process_from_code("/main.c", main)
            .await
            .unwrap();
        assert_eq!(
            on_b.rx.try_recv(),
            Ok(ConnectionOp::SendMessage(super::DISPLACED.into()))
        );
        assert_eq!(on_b.rx.try_recv(), Ok(ConnectionOp::Close));
        assert!(on_b.connection.body().is_none());
        assert_eq!(on_a.rx.try_recv(), Ok(ConnectionOp::Attached));
        assert!(on_a.rx.try_recv().is_err());
        assert_eq!(
            on_a.connection.body().as_ref().map(|p| p.to_string()),
            Some("/b".to_owned())
        );
    }

    #[tokio::test]
    async fn a_body_cannot_exec_into_itself() {
        let vm = Vm::new(test_config());
        let a = vm.create_process_from_code("/a.c", "").await.unwrap();
        let mut on_a = connect(&vm, &a).await;
        let main = indoc! { r#"
            void create() { exec(find_object("/a"), find_object("/a")); }
        "# };
        let err = vm
            .initialize_process_from_code("/main.c", main)
            .await
            .unwrap_err();
        assert!(
            err.to_string()
                .contains("exec: `new` and `old` are the same object"),
            "{err}"
        );
        assert!(on_a.rx.try_recv().is_err(), "nothing reaches the player");
        assert!(
            vm.global_state
                .committed_connection(&a)
                .is_some_and(|held| Arc::ptr_eq(&held, &on_a.connection)),
            "the cell still holds the connection"
        );
        assert_eq!(
            on_a.connection.body().as_ref().map(|p| p.to_string()),
            Some("/a".to_owned())
        );
    }

    #[tokio::test]
    async fn an_exec_from_a_body_that_is_not_interactive_fails() {
        let vm = Vm::new(test_config());
        vm.create_process_from_code("/a.c", "").await.unwrap();
        vm.create_process_from_code("/b.c", "").await.unwrap();
        let main = indoc! { r#"
            void create() {
                if (exec(find_object("/b"), find_object("/a")) != 0) {
                    throw("a body with no connection has nothing to move");
                }
            }
        "# };
        vm.initialize_process_from_code("/main.c", main)
            .await
            .unwrap();
    }

    /// Build a [`Connection`] whose own channels are dropped after the test.
    fn make_connection() -> Arc<Connection> {
        let (tx, _rx) = tokio::sync::mpsc::unbounded_channel();
        Arc::new(Connection::new(
            "127.0.0.1:23123".to_socket_addrs().unwrap().next().unwrap(),
            tx,
        ))
    }

    /// The core D9b Piece 2 guarantee: an in-transaction `exec` is visible
    /// to `interactive()` within the same transaction. The old body's
    /// `create()` starts bound (via `GlobalState::attach`), then `exec`s the
    /// connection into the target. The `interactive(target)` reads that
    /// follow run in the same transaction the `exec` wrote the cell into,
    /// i.e. before any commit — they must still observe the handover.
    #[tokio::test]
    async fn exec_binding_visible_in_same_transaction() {
        let old_body = indoc! { r#"
            int create() {
                object ob = find_object("/target.c");

                if (interactive() != 1) {
                    throw("old body must start bound");
                }
                if (interactive(ob) != 0) {
                    throw("target unbound before exec");
                }
                if (exec(ob, this_object()) != 1) {
                    throw("exec failed");
                }
                // Same transaction as the exec above, no commit between:
                if (interactive(ob) != 1) {
                    throw("target must see the in-transaction exec");
                }
                if (interactive() != 0) {
                    throw("old body must be unbound after exec");
                }
                return 1;
            }
        "# };

        let target = indoc! { r#"
            int create() {
                return interactive();
            }
        "# };

        let vm = Vm::new(test_config());
        let connection = make_connection();

        let target_proc = vm
            .create_process_from_code("/target.c", target)
            .await
            .unwrap();

        let old_proc = vm
            .create_process_from_code("/old_body.c", old_body)
            .await
            .unwrap();

        // Bind the connection to the old body through the transactional
        // path (the login mechanism), so the cell is in the committed world.
        vm.global_state
            .attach(connection.clone(), old_proc.clone())
            .await;

        // Run the old body's `create()`: it performs the exec and makes the
        // in-transaction visibility assertions above.
        let task = Task::<16>::initialize_process(
            TaskTemplate::from(vm.global_state.clone()).into_task_context(old_proc),
        )
        .await
        .unwrap();
        let result = task.result().unwrap();
        assert_eq!(result, LpcRef::Int(LpcInt(1)));

        // After the commit, the binding is settled in the committed world:
        // a fresh task on the target must still see it (its `create()`
        // returns `interactive()`).
        let task = Task::<16>::initialize_process(
            TaskTemplate::from(vm.global_state.clone()).into_task_context(target_proc),
        )
        .await
        .unwrap();
        let result = task.result().unwrap();
        assert_eq!(result, LpcRef::Int(LpcInt(1)));
    }
}
