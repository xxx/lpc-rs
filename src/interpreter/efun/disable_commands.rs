use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, process::Process};

/// `disable_commands`, an efun that disables an object from being able to interact with the game world.
pub async fn disable_commands<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let proc = &context.frame().process;
    let was_enabled = proc.commands_enabled(context.txn());
    let environment = Process::environment_of(context.txn(), proc);

    context.txn().with(|t| {
        t.drop_var(proc.commands_enabled.id);
        t.drop_var(proc.rules.id);
        if was_enabled && let Some(env) = environment {
            Process::unmark_living(t, proc, &env);
        }
    });

    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{
            CommittedReader,
            task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
            vm::Vm,
        },
        test_support::test_config,
    };

    #[tokio::test]
    async fn test_disable_commands() {
        let master = indoc! { r#"
            void create() {
                "/maybe_interactive"->set_interactive();
            }

            void disabler() {
                "/maybe_interactive"->disable_interactive();
            }
        "# };

        let maybe_interactive = indoc! { r#"
            void set_interactive() {
                enable_commands();
            }

            void disable_interactive() {
                disable_commands();
            }
        "# };

        let vm = Vm::new(test_config());
        let _maybe_interactive_proc = vm
            .create_process_from_code("/maybe_interactive.c", maybe_interactive)
            .await
            .unwrap();

        let master_proc = vm
            .initialize_process_from_code("master.c", master)
            .await
            .unwrap();

        let prototype = master_proc
            .context
            .object_space()
            .lookup("/maybe_interactive")
            .unwrap()
            .clone();

        assert!(vm.global_state.commands_enabled(&prototype));

        apply_function_by_name(
            "disabler",
            &[],
            master_proc.context.process,
            TaskTemplate::from(vm.global_state.clone()),
            None,
        )
        .await
        .unwrap()
        .unwrap();

        assert!(!vm.global_state.commands_enabled(&prototype));
    }
}
