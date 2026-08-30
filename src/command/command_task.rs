//! The telnet path's command: one transaction per line, retried as a unit,
//! with the dispatch pipeline as its attempt body.

use std::sync::Arc;

use lpc_rs_errors::{Result, lpc_bug};

use crate::{
    command::dispatch::{Outcome, dispatch_from_connection},
    interpreter::{
        process::Process,
        stm::{
            AttemptBody, CommitProtocol, Conflict, Effect, LiveSnapshot, Transaction, TxnHandle,
            commit_changeset, flush_effects, run_attempts, start_txn,
        },
        task::task_template::TaskTemplate,
        task_context::TaskContext,
    },
};

/// One command line from a connection, run as its own transaction.
pub(crate) struct CommandTask {
    template: TaskTemplate,
    actor: Arc<Process>,
    line: String,
    /// The context of the attempt in flight; what the commit phase reads.
    context: Option<TaskContext>,
    outcome: Outcome,
}

impl CommandTask {
    /// Build a task for `line`, run by `actor`, from `template`.
    pub(crate) fn new(template: TaskTemplate, actor: Arc<Process>, line: String) -> Self {
        Self {
            template,
            actor,
            line,
            context: None,
            outcome: Outcome::Unhandled,
        }
    }
}

#[async_trait::async_trait]
impl AttemptBody for CommandTask {
    async fn begin_attempt(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
    ) -> Result<Option<LiveSnapshot>> {
        let live = start_txn(tx).await?;
        let mut template = self.template.clone();
        template.txn = TxnHandle::new(Transaction::new(live.inner.clone()));
        template.set_this_player(Some(self.actor.clone()));
        let ctx = template.into_task_context(self.actor.clone());
        match dispatch_from_connection(&ctx, self.actor.clone(), &self.line).await {
            Ok(outcome) => {
                self.outcome = outcome;
                self.context = Some(ctx);
                Ok(Some(live))
            }
            Err(e) => {
                // A failed attempt holds nothing the committer needs.
                drop(live);
                Err(e)
            }
        }
    }

    async fn commit_phase(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
        _live: LiveSnapshot,
    ) -> Result<(std::result::Result<(), Conflict>, Vec<Effect>)> {
        let Some(ctx) = &self.context else {
            return Err(lpc_bug!("a command attempt commits without a context"));
        };
        let changeset = ctx.txn().with(|t| t.clone_changeset());
        let commit = commit_changeset(tx, changeset).await?;
        let effects = ctx.txn().with(|t| t.take_effects());
        Ok((commit, effects))
    }

    async fn deliver(&mut self, effects: Vec<Effect>) -> Result<()> {
        if effects.is_empty() {
            return Ok(());
        }
        let global_state = &self.template.global_state;
        flush_effects(global_state, effects).await;
        Ok(())
    }
}

/// Run `line` as `actor` in a transaction of its own, retried on conflict.
pub(crate) async fn run_command_line(
    template: &TaskTemplate,
    actor: Arc<Process>,
    line: String,
) -> Result<Outcome> {
    let mut task = CommandTask::new(template.clone(), actor, line);
    let global_state = template.global_state.clone();
    let (result, _stats) = run_attempts(
        &global_state.committer_tx,
        &global_state.attempt_telemetry,
        Some(global_state.commit_watch.clone()),
        &mut task,
    )
    .await;
    result.map(|()| task.outcome)
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use super::*;
    use crate::{
        interpreter::{
            CommittedReader, lpc_ref::LpcRef, lpc_string::LpcString,
            task::task_template::TaskTemplate, vm::Vm,
        },
        test_support::test_config,
    };

    const PLAYER: &str = indoc! { r#"
        string seen; string heard;
        void create() { set_this_player(this_object()); enable_commands(); add_action("do_look", "look"); }
        int do_look(string arg) { seen = arg; return 1; }
        void catch_tell(string m) { heard = m; }
    "# };

    #[tokio::test]
    async fn a_line_commits_its_handlers_writes() {
        let vm = Vm::new(test_config());
        let player = vm
            .initialize_process_from_code("/player.c", PLAYER)
            .await
            .unwrap()
            .context
            .process;
        let template = TaskTemplate::from(vm.global_state.clone());
        let outcome = run_command_line(&template, player.clone(), "look at me".into())
            .await
            .unwrap();
        assert_eq!(outcome, Outcome::Handled);
        assert_eq!(
            vm.global_state.committed_global(&player, 0u16),
            LpcRef::from(LpcString::from("at me"))
        );
    }

    #[tokio::test]
    async fn an_unhandled_line_delivers_the_fallback() {
        let vm = Vm::new(test_config());
        let player = vm
            .initialize_process_from_code("/player.c", PLAYER)
            .await
            .unwrap()
            .context
            .process;
        let template = TaskTemplate::from(vm.global_state.clone());
        let outcome = run_command_line(&template, player.clone(), "dance".into())
            .await
            .unwrap();
        assert_eq!(outcome, Outcome::Unhandled);
        assert_eq!(
            vm.global_state.committed_global(&player, 1u16),
            LpcRef::from(LpcString::from("What?\n"))
        );
    }

    #[tokio::test]
    async fn a_body_that_never_enabled_commands_hears_the_hint() {
        let code = indoc! { r#"
            string heard;
            void create() {}
            void catch_tell(string m) { heard = m; }
        "# };
        let vm = Vm::new(test_config());
        let player = vm
            .initialize_process_from_code("/player.c", code)
            .await
            .unwrap()
            .context
            .process;
        let template = TaskTemplate::from(vm.global_state.clone());
        let outcome = run_command_line(&template, player.clone(), "dance".into())
            .await
            .unwrap();
        assert_eq!(outcome, Outcome::Unhandled);
        let heard = vm.global_state.committed_global(&player, 0u16).to_string();
        assert!(heard.contains("enable_commands()"), "{heard}");
    }

    #[tokio::test]
    async fn a_body_whose_process_input_consumes_the_line_hears_nothing() {
        let code = indoc! { r#"
            mixed heard; string seen;
            void create() {}
            int process_input(string line) { seen = line; return 1; }
            void catch_tell(string m) { heard = m; }
        "# };
        let vm = Vm::new(test_config());
        let player = vm
            .initialize_process_from_code("/player.c", code)
            .await
            .unwrap()
            .context
            .process;
        let template = TaskTemplate::from(vm.global_state.clone());
        let outcome = run_command_line(&template, player.clone(), "dance".into())
            .await
            .unwrap();
        assert_eq!(outcome, Outcome::Handled);
        assert_eq!(
            vm.global_state.committed_global(&player, 0u16),
            LpcRef::from(0)
        );
        assert_eq!(
            vm.global_state.committed_global(&player, 1u16),
            LpcRef::from(LpcString::from("dance"))
        );
    }

    #[tokio::test]
    async fn a_handler_error_surfaces_and_commits_nothing() {
        let code = indoc! { r#"
            int touched;
            void create() { set_this_player(this_object()); enable_commands(); add_action("do_boom", "boom"); }
            int do_boom(string arg) { touched = 1; int j; return 1 / j; }
        "# };
        let vm = Vm::new(test_config());
        let player = vm
            .initialize_process_from_code("/player.c", code)
            .await
            .unwrap()
            .context
            .process;
        let template = TaskTemplate::from(vm.global_state.clone());
        run_command_line(&template, player.clone(), "boom".into())
            .await
            .expect_err("the handler divides by zero");
        assert_eq!(
            vm.global_state.committed_global(&player, 0u16),
            LpcRef::from(0)
        );
    }
}
