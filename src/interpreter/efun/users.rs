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
            process.is_live(txn)
                && txn
                    .with(|t| t.read_connection(process.connection.id))
                    .is_some()
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
        interpreter::vm::Vm,
        test_support::{connect, test_config},
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
        let a = vm.create_process_from_code("/a.c", "").await.unwrap();
        let _a = connect(&vm, &a).await;
        let code = r#"object *create() { destruct(find_object("/a")); return users(); }"#;
        let users = users_seen_by(&vm, code).await;
        assert!(users.is_empty(), "{users:?}");
    }
}
