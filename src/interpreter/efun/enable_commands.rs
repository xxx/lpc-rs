use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef, process::Process};

/// `enable_commands`, an efun that enables an object to interact with the game world.
pub async fn enable_commands<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let proc = &context.frame().process;
    let already = proc.commands_enabled(context.txn());
    let environment = Process::environment_of(context.txn(), proc);

    context.txn().with(|t| {
        t.write(proc.commands_enabled.id, LpcRef::from(1));
        if !already && let Some(env) = environment {
            Process::mark_living(t, proc, &env);
        }
    });

    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{CommittedReader, lpc_ref::LpcRef, vm::Vm},
        test_support::test_config,
    };

    #[tokio::test]
    async fn a_failed_attempt_does_not_enable_commands() {
        let ob = indoc! { r#"
            void create() {
                enable_commands();
                int j = 0;
                int x = 10 / j;
            }
        "# };
        let checker = indoc! { r#"
            int alive = living("/ob");
        "# };

        let vm = Vm::new(test_config());
        vm.initialize_process_from_code("/ob.c", ob)
            .await
            .expect_err("the initializer divides by zero");
        let checker_proc = vm
            .initialize_process_from_code("/checker.c", checker)
            .await
            .unwrap()
            .context
            .process;

        assert_eq!(
            vm.global_state.committed_global(&checker_proc, 0u16),
            LpcRef::from(0)
        );
    }

    #[tokio::test]
    async fn test_enable_commands() {
        let master = indoc! { r#"
            void create() {
                "/maybe_interactive"->set_interactive();

                clone_object("/maybe_interactive");
            }
        "# };

        let maybe_interactive = indoc! { r#"
            void set_interactive() {
                enable_commands();
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

        let space = master_proc.context.object_space();
        let prototype = space.lookup("/maybe_interactive").unwrap();
        let clone = space.lookup("/maybe_interactive#0").unwrap();

        assert!(vm.global_state.commands_enabled(&prototype));
        assert!(!vm.global_state.commands_enabled(&clone));
    }
}
