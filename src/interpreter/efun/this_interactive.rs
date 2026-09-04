use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// `this_interactive()`: the command giver the task started with, while it
/// has a connection; `set_this_player` never moves it.
pub fn this_interactive<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let txn = context.txn();
    let result = context
        .task_context()
        .entry_player
        .as_ref()
        .filter(|player| {
            player.is_live(txn)
                && txn
                    .with(|t| t.read_connection(player.connection.id))
                    .is_some()
        })
        .map_or(LpcRef::from(0), |player| {
            LpcRef::from(Arc::downgrade(player))
        });
    context.return_efun_result(result);
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use crate::{
        interpreter::{
            lpc_ref::LpcRef, process::Process, task::Task, task::task_template::TaskTemplate,
            vm::Vm,
        },
        test_support::{connect, run_prog, test_config},
    };

    /// Run `code` as a task whose command giver is `player`.
    async fn result_with_player(vm: &Vm, player: Option<Arc<Process>>, code: &str) -> LpcRef {
        let process = vm.create_process_from_code("/t.c", code).await.unwrap();
        let template = TaskTemplate::from(vm.global_state.clone());
        template.set_this_player(player);
        let task = Task::<16>::initialize_process(template.into_task_context(process))
            .await
            .unwrap();
        task.result().expect("a result")
    }

    #[tokio::test]
    async fn this_interactive_is_the_connected_player_the_task_started_with() {
        let vm = Vm::new(test_config());
        let player = vm.create_process_from_code("/player.c", "").await.unwrap();
        let _connected = connect(&vm, &player).await;
        let code = "object create() { return this_interactive(); }";
        let result = result_with_player(&vm, Some(player.clone()), code).await;
        assert_eq!(result, LpcRef::from(Arc::downgrade(&player)));
    }

    #[tokio::test]
    async fn set_this_player_does_not_move_this_interactive() {
        let vm = Vm::new(test_config());
        let player = vm.create_process_from_code("/player.c", "").await.unwrap();
        let _connected = connect(&vm, &player).await;
        let code = "object create() { set_this_player(this_object()); return this_interactive(); }";
        let result = result_with_player(&vm, Some(player.clone()), code).await;
        assert_eq!(result, LpcRef::from(Arc::downgrade(&player)));
    }

    #[tokio::test]
    async fn a_player_without_a_connection_is_not_interactive() {
        let vm = Vm::new(test_config());
        let npc = vm.create_process_from_code("/npc.c", "").await.unwrap();
        let code = "object create() { return this_interactive(); }";
        let result = result_with_player(&vm, Some(npc), code).await;
        assert_eq!(result, LpcRef::from(0));
    }

    #[tokio::test]
    async fn this_interactive_is_zero_without_a_player() {
        let code = "object create() { return this_interactive(); }";
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(0)));
    }
}
