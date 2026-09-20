use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// `users()`: every live object with a connection, the login object of a
/// connection still at `logon` included; the physical space walked, the
/// binding read through this attempt.
pub fn users<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let txn = context.txn();
    let bodies: Vec<LpcRef> = context
        .object_space()
        .iter()
        .map(|entry| entry.value().clone())
        .filter(|process| {
            // Checking liveness first makes unrelated object loads invalidate this attempt.
            txn.with(|t| t.read_connection(process.connection.id))
                .is_some()
                && process.is_live(txn)
        })
        .map(|process| LpcRef::from(Arc::downgrade(&process)))
        .collect();
    context.return_array(bodies);
    Ok(())
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::{
        interpreter::{
            lpc_ref::LpcRef,
            stm::{Conflict, Transaction, TxnHandle, commit_changeset, start_txn},
            task::Task,
            task_context::TaskContext,
            vm::Vm,
        },
        test_support::{allow_exec, connect, permissive_master, test_config},
    };

    async fn users_seen_by(vm: &Vm, code: &str) -> Vec<String> {
        let task = vm
            .initialize_process_from_code("/watcher.c", code)
            .await
            .unwrap();
        task.result()
            .unwrap()
            .with_array(task.context.txn(), |arr| {
                arr.iter().map(|x| x.to_string()).sorted().collect()
            })
            .unwrap()
    }

    #[tokio::test]
    async fn users_are_the_bodies_of_every_live_connection() {
        let vm = Vm::new(test_config());
        let a = vm.create_process_from_code("/a.c", "").await.unwrap();
        let b = vm.create_process_from_code("/b.c", "").await.unwrap();
        let _a = connect(&vm, &a).await;
        let _b = connect(&vm, &b).await;
        let users = users_seen_by(&vm, "object *create() { return users(); }").await;
        assert_eq!(users, ["/a", "/b"]);
    }

    #[tokio::test]
    async fn users_is_empty_with_no_connections() {
        let vm = Vm::new(test_config());
        let users = users_seen_by(&vm, "object *create() { return users(); }").await;
        assert!(users.is_empty(), "{users:?}");
    }

    #[tokio::test]
    async fn a_body_destructed_in_this_task_is_not_a_user() {
        let vm = Vm::new(test_config());
        crate::test_support::allow_destruct(&vm).await;
        let a = vm.create_process_from_code("/a.c", "").await.unwrap();
        let _a = connect(&vm, &a).await;
        let code = r#"object *create() { destruct(find_object("/a")); return users(); }"#;
        let users = users_seen_by(&vm, code).await;
        assert!(users.is_empty(), "{users:?}");
    }

    #[tokio::test]
    async fn an_unconnected_object_loaded_after_the_snapshot_does_not_force_a_retry() {
        let vm = Vm::new(test_config());
        permissive_master(&vm.global_state.object_space).await;
        let login = vm.create_process_from_code("/login.c", "").await.unwrap();
        let _connection = connect(&vm, &login).await;
        let watcher = vm
            .create_process_from_code("/watcher.c", "object *create() { return users(); }")
            .await
            .unwrap();
        let tx = &vm.global_state.committer_tx;
        let mut live = start_txn(tx).await.unwrap();
        let mut context = TaskContext::new(vm.global_state.clone(), watcher, None);
        context.txn = TxnHandle::new(Transaction::new(live.inner.clone()));

        vm.initialize_process_from_code("/loader.c", r#"void create() { load_object("/empty"); }"#)
            .await
            .unwrap();
        let loaded = vm.global_state.object_space.lookup("/empty").unwrap();
        assert!(loaded.cell.get().is_some());

        let task = Task::<32>::initialize_process(context).await.unwrap();
        let users = task
            .result()
            .unwrap()
            .with_array(task.context.txn(), |arr| {
                arr.iter().map(|user| user.to_string()).collect::<Vec<_>>()
            })
            .unwrap();
        assert_eq!(users, ["/login"]);
        let changeset = task.context.txn().with(|txn| txn.take_changeset());
        live.disarm();
        assert_eq!(commit_changeset(tx, changeset).await.unwrap(), Ok(()));
    }

    #[tokio::test]
    async fn a_connection_attached_after_the_snapshot_still_forces_a_retry() {
        let vm = Vm::new(test_config());
        let login = vm.create_process_from_code("/login.c", "").await.unwrap();
        let watcher = vm
            .create_process_from_code("/watcher.c", "int create() { return sizeof(users()); }")
            .await
            .unwrap();
        let tx = &vm.global_state.committer_tx;
        let mut live = start_txn(tx).await.unwrap();
        let mut context = TaskContext::new(vm.global_state.clone(), watcher, None);
        context.txn = TxnHandle::new(Transaction::new(live.inner.clone()));

        let _connection = connect(&vm, &login).await;
        let task = Task::<32>::initialize_process(context).await.unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(0)));
        let changeset = task.context.txn().with(|txn| txn.take_changeset());
        live.disarm();
        assert!(matches!(
            commit_changeset(tx, changeset).await.unwrap(),
            Err(Conflict::ReadInvalidated { cell, .. }) if cell == login.connection.id
        ));
    }

    #[tokio::test]
    async fn users_sees_an_uncommitted_connection_handover() {
        let vm = Vm::new(test_config());
        allow_exec(&vm).await;
        let login = vm.create_process_from_code("/login.c", "").await.unwrap();
        vm.create_process_from_code("/player.c", "").await.unwrap();
        let _connection = connect(&vm, &login).await;
        let users = users_seen_by(
            &vm,
            r#"object *create() {
                exec(find_object("/player"), find_object("/login"));
                return users();
            }"#,
        )
        .await;
        assert_eq!(users, ["/player"]);
    }
}
