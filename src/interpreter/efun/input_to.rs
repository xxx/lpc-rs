use lpc_rs_errors::Result;
use tracing::trace;

use crate::{
    interpreter::{
        efun::{add_action::handler_from, efun_context::EfunContext},
        lpc_ref::LpcRef,
        stm::Effect,
    },
    telnet::{connection::InputTo, ops::ConnectionOp},
};

/// `input_to`, an efun for registering a function to be called when the user
/// types something into the game.
pub fn input_to<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let ptr = handler_from(context, context.arg(0).clone(), "input_to")?;

    let LpcRef::Int(no_echo) = context.arg(1) else {
        return Err(context.runtime_error("non-integer sent as second argument to `input_to`"));
    };

    let input_to = InputTo {
        ptr,
        no_echo: (*no_echo).into(),
    };

    let process = context.this_player().load();

    if let Some(process) = &*process
        && let Some(connection) = context
            .txn()
            .with(|t| t.read_connection(process.connection.id))
    {
        context.record_effect(Effect::Socket {
            op: ConnectionOp::InputTo(input_to),
            tx: connection.sender(),
        });
        context.return_efun_result(LpcRef::from(1));
        return Ok(());
    } else {
        // No connection to receive the message, so do nothing.
        trace!("input_to on non-interactive process");
        // 0 is already returned by default
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use indoc::indoc;
    use lpc_rs_core::null_mode::NullMode;
    use lpc_rs_utils::config::ConfigBuilder;

    use crate::{
        interpreter::{
            CommittedReader,
            lpc_ref::LpcRef,
            process::Process,
            stm::{CommitProtocol, Committer, Transaction},
            task::{
                SeedEntry, TaskSeed, apply_function::apply_function_by_name,
                task_template::TaskTemplate,
            },
            vm::Vm,
        },
        telnet::ops::ConnectionOp,
        test_config_builder,
        test_support::{Connected, allow_exec, connect, test_config},
    };

    async fn connected(mode: NullMode, code: &str) -> (Vm, Arc<Process>, Connected) {
        let vm = Vm::new(test_config_builder!().null_mode(mode).build().unwrap());
        let process = vm
            .initialize_process_from_code("/asker.c", code)
            .await
            .unwrap()
            .context
            .process;
        let connection = connect(&vm, &process).await;
        (vm, process, connection)
    }

    async fn call(vm: &Vm, process: &Arc<Process>, name: &str) -> lpc_rs_errors::Result<LpcRef> {
        let template = TaskTemplate::from(vm.global_state.clone());
        template.set_this_player(Some(process.clone()));
        apply_function_by_name(name, &[], process.clone(), template, Some(5_000))
            .await
            .unwrap()
    }

    #[tokio::test]
    async fn input_to_commits_in_order_and_preserves_captured_data() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let (vm, process, mut connected) = connected(
                mode,
                r#"
                string result;
                void heard(string *captured, string line) { result = captured[0] + line; }
                int arm() {
                    string *captured = ({ "captured:" });
                    write_socket("before");
                    int registered = input_to((: heard(captured, $1) :), 1);
                    write_socket("after");
                    return registered;
                }
                string check() { return result; }
            "#,
            )
            .await;
            assert_eq!(call(&vm, &process, "arm").await.unwrap(), LpcRef::from(1));
            assert_eq!(
                connected.rx.try_recv().unwrap(),
                ConnectionOp::SendMessage("before".into())
            );
            let ConnectionOp::InputTo(input) = connected.rx.try_recv().unwrap() else {
                panic!("registration follows the first message");
            };
            assert!(input.no_echo);
            assert_eq!(
                connected.rx.try_recv().unwrap(),
                ConnectionOp::SendMessage("after".into())
            );
            assert!(connected.rx.try_recv().is_err());
            connected.connection.set_input_to(Some(input));
            vm.global_state.gc().await.unwrap().unwrap();
            let input = connected.connection.take_input_to().unwrap();
            vm.global_state
                .prepare_function_ptr(
                    &input.ptr,
                    &[LpcRef::from("line")],
                    connected.connection.body(),
                )
                .await
                .unwrap()
                .unwrap()
                .execute(5_000)
                .await
                .unwrap();
            assert_eq!(
                call(&vm, &process, "check").await.unwrap(),
                LpcRef::from("captured:line")
            );
        }
    }

    #[tokio::test]
    async fn failed_input_to_replacement_leaves_the_old_handler_and_state() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let (vm, process, mut connected) = connected(
                mode,
                r#"
                int state;
                void original(string line) { state = 7; }
                void replacement(string line) { state = 99; }
                void arm() { input_to(original); }
                void fail() { state = 1; input_to(replacement, 1); throw("abort input"); }
                int check() { return state; }
            "#,
            )
            .await;
            call(&vm, &process, "arm").await.unwrap();
            let ConnectionOp::InputTo(input) = connected.rx.try_recv().unwrap() else {
                panic!("original registration");
            };
            connected.connection.set_input_to(Some(input));
            assert!(
                call(&vm, &process, "fail")
                    .await
                    .unwrap_err()
                    .to_string()
                    .contains("abort input")
            );
            assert_eq!(call(&vm, &process, "check").await.unwrap(), LpcRef::from(0));
            assert!(
                connected.rx.try_recv().is_err(),
                "failed registration must never reach the connection"
            );
            let input = connected.connection.take_input_to().unwrap();
            assert!(!input.no_echo);
            vm.global_state
                .prepare_function_ptr(
                    &input.ptr,
                    &[LpcRef::from("line")],
                    connected.connection.body(),
                )
                .await
                .unwrap()
                .unwrap()
                .execute(5_000)
                .await
                .unwrap();
            assert_eq!(call(&vm, &process, "check").await.unwrap(), LpcRef::from(7));
        }
    }

    #[tokio::test]
    async fn caught_errors_keep_input_to_in_the_committing_transaction() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let (vm, process, mut connected) = connected(
                mode,
                r#"
                void heard(string line) {}
                void fail() { input_to(heard); throw("caught"); }
                int arm() { return !!catch(fail()); }
            "#,
            )
            .await;
            assert_eq!(call(&vm, &process, "arm").await.unwrap(), LpcRef::from(1));
            assert!(matches!(
                connected.rx.try_recv(),
                Ok(ConnectionOp::InputTo(_))
            ));
            assert!(connected.rx.try_recv().is_err());
        }
    }

    #[tokio::test]
    async fn input_to_follows_a_transactional_connection_handover() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let (vm, process, mut connected) = connected(
                mode,
                r#"
                int result;
                void heard(string line) {
                    result = previous_object() == this_object()
                        && file_name(this_player()) == "/next" && line == "line";
                }
                int arm() {
                    if (!exec(find_object("/next"), this_object())) throw("handover refused");
                    return input_to(heard);
                }
                int check() { return result; }
            "#,
            )
            .await;
            allow_exec(&vm).await;
            let next = vm
                .initialize_process_from_code("/next.c", "")
                .await
                .unwrap()
                .context
                .process;
            assert_eq!(call(&vm, &process, "arm").await.unwrap(), LpcRef::from(1));
            assert_eq!(connected.rx.try_recv().unwrap(), ConnectionOp::Attached);
            assert!(Arc::ptr_eq(&connected.connection.body().unwrap(), &next));
            let ConnectionOp::InputTo(input) = connected.rx.try_recv().unwrap() else {
                panic!("registration follows the handover");
            };
            assert!(connected.rx.try_recv().is_err());
            vm.global_state
                .prepare_function_ptr(
                    &input.ptr,
                    &[LpcRef::from("line")],
                    connected.connection.body(),
                )
                .await
                .unwrap()
                .unwrap()
                .execute(5_000)
                .await
                .unwrap();
            assert_eq!(call(&vm, &process, "check").await.unwrap(), LpcRef::from(1));
        }
    }

    #[tokio::test]
    async fn retried_input_to_is_published_only_once() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let vm = Vm::new(test_config_builder!().null_mode(mode).build().unwrap());
            let mut task = vm
                .initialize_process_from_code(
                    "/asker.c",
                    r#"
                void heard(string line) {}
                int arm() { set_this_player(this_object()); return input_to(heard); }
            "#,
                )
                .await
                .unwrap();
            let process = task.context.process.clone();
            let mut connected = connect(&vm, &process).await;
            let mut committer = Committer::with_null_mode(mode);
            let mut transaction = Transaction::new(committer.snapshot_clone());
            transaction.write_connection(process.connection.id, Some(connected.connection.clone()));
            committer.commit(transaction.into_parts().1).unwrap();
            let seed = TaskSeed {
                entry: SeedEntry::Function(
                    process
                        .initial_program()
                        .lookup_function("arm")
                        .unwrap()
                        .clone(),
                ),
                process,
                args: Vec::new(),
                initializes: false,
            };
            let (tx, rx) = flume::bounded(4);
            let committer_tx = tx.clone();
            let handle =
                std::thread::spawn(move || committer.run_with_rejections(committer_tx, rx, 1));
            let (result, stats) = task.eval_with_committer(&tx, &seed).await;
            tx.send(CommitProtocol::Close).unwrap();
            handle.join().unwrap();
            result.unwrap();
            assert_eq!((stats.attempts, stats.conflicts), (2, 1));
            assert_eq!(task.result(), Some(LpcRef::from(1)));
            assert!(matches!(
                connected.rx.try_recv(),
                Ok(ConnectionOp::InputTo(_))
            ));
            assert!(
                connected.rx.try_recv().is_err(),
                "a rejected attempt must publish nothing"
            );
        }
    }

    async fn error_of(code: &str) -> String {
        Vm::new(test_config())
            .initialize_process_from_code("/asker.c", code)
            .await
            .unwrap_err()
            .to_string()
    }

    #[tokio::test]
    async fn a_name_the_object_lacks_is_an_error() {
        let code = indoc! { r#"
            void create() { input_to("nope"); }
        "# };
        let err = error_of(code).await;
        assert!(
            err.contains("input_to: no function `nope` in /asker"),
            "{err}"
        );
    }

    #[tokio::test]
    async fn an_unbound_pointer_is_an_error() {
        let code = indoc! { r#"
            void create() { input_to(&->heard()); }
        "# };
        let err = error_of(code).await;
        assert!(err.contains("input_to: the receiver"), "{err}");
    }

    #[tokio::test]
    async fn a_name_without_a_player_registers_nothing() {
        let code = indoc! { r#"
            int r = -1;
            void create() { r = input_to("heard"); }
            void heard(string s) {}
        "# };
        let vm = Vm::new(test_config());
        let proc = vm
            .initialize_process_from_code("/asker.c", code)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            vm.global_state.committed_global(&proc, 0u16),
            LpcRef::from(0)
        );
    }
}
