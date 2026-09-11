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
    fn timeout_ms(&self) -> u64 {
        self.template.global_state.config.max_execution_time
    }

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
    use lpc_rs_utils::lpc_string::LpcString;

    use super::*;
    use crate::{
        interpreter::{
            CommittedReader, lpc_ref::LpcRef, task::task_template::TaskTemplate, vm::Vm,
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

    #[tokio::test]
    async fn a_caught_missing_semicolon_cannot_retry_the_command_forever() {
        use std::time::Duration;

        use lpc_rs_utils::config::ConfigBuilder;

        use crate::{
            interpreter::stm::committer_stats,
            test_support::{TempLib, permissive_master},
        };

        struct ConflictedCommand(CommandTask);

        #[async_trait::async_trait]
        impl AttemptBody for ConflictedCommand {
            fn timeout_ms(&self) -> u64 {
                self.0.timeout_ms()
            }

            async fn begin_attempt(
                &mut self,
                tx: &flume::Sender<CommitProtocol>,
            ) -> Result<Option<LiveSnapshot>> {
                self.0.begin_attempt(tx).await
            }

            async fn commit_phase(
                &mut self,
                tx: &flume::Sender<CommitProtocol>,
                live: LiveSnapshot,
            ) -> Result<(std::result::Result<(), Conflict>, Vec<Effect>)> {
                let mut concurrent = start_txn(tx).await?;
                let mut txn = Transaction::new(concurrent.inner.clone());
                txn.write(self.0.actor.var_id(0), LpcRef::from(0));
                concurrent.disarm();
                txn.commit(tx).await?.0.unwrap();
                self.0.commit_phase(tx, live).await
            }

            async fn deliver(&mut self, effects: Vec<Effect>) -> Result<()> {
                self.0.deliver(effects).await
            }
        }

        let root = TempLib::new("command-compile-error-retries");
        std::fs::write(root.join("cover.c"), "int move() { return 1 }").unwrap();
        std::fs::write(root.join("desk.c"), "inherit \"/cover\";").unwrap();
        std::fs::write(
            root.join("room.c"),
            "void create() { clone_object(\"/desk\"); }",
        )
        .unwrap();
        let config = ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .max_execution_time(1000_u64)
            .build()
            .unwrap();
        let vm = Vm::new(config);
        permissive_master(&vm.global_state.object_space).await;
        let player = vm
            .initialize_process_from_code(
                "/player.c",
                indoc! { r#"
            int touched;
            string error;
            void create() {
                enable_commands();
                set_this_player(this_object());
                add_action("north", "north");
                add_action("look", "look");
            }
            int north() {
                if (touched) return 0;
                touched = 1;
                error = catch(call_other("/room", "??"));
                return 1;
            }
            int look() { touched = 2; return 1; }
        "# },
            )
            .await
            .unwrap()
            .context
            .process;
        let template = TaskTemplate::from(vm.global_state.clone());
        let mut command = ConflictedCommand(CommandTask::new(
            template.clone(),
            player.clone(),
            "north".into(),
        ));
        let (result, stats) = tokio::time::timeout(
            Duration::from_secs(5),
            run_attempts(
                &vm.global_state.committer_tx,
                &vm.global_state.attempt_telemetry,
                Some(vm.global_state.commit_watch.clone()),
                &mut command,
            ),
        )
        .await
        .expect("compile-error retries must honor the command's execution limit");
        assert_eq!(
            result.unwrap_err().to_string(),
            "evaluation limit of 1000ms has been reached"
        );
        assert!(stats.conflicts > 1, "{stats:?}");
        let ctx = command.0.context.as_ref().unwrap();
        let caught = ctx.txn().with(|txn| txn.read(player.var_id(1)).unwrap());
        assert!(
            caught.to_string().contains("Unrecognized Token"),
            "{caught}"
        );
        assert!(caught.to_string().contains("/cover.c"), "{caught}");
        for slot in [0, 1] {
            assert_eq!(
                vm.global_state.committed_global(&player, slot),
                LpcRef::from(0)
            );
        }
        assert!(vm.global_state.object_space.lookup("/room").is_none());
        drop(command);
        assert_eq!(
            committer_stats(&vm.global_state.committer_tx)
                .await
                .unwrap()
                .live_snapshots,
            0
        );
        assert_eq!(
            run_command_line(&template, player.clone(), "look".into())
                .await
                .unwrap(),
            Outcome::Handled
        );
        assert_eq!(
            vm.global_state.committed_global(&player, 0),
            LpcRef::from(2)
        );
    }
}
