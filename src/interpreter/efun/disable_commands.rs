use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, object_flags::ObjectFlags};

/// `disable_commands`, an efun that disables an object from being able to interact with the game world.
pub async fn disable_commands<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let proc = &context.frame().process;

    proc.flags.clear(ObjectFlags::CommandsEnabled);

    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{
            object_flags::ObjectFlags, task::apply_function::apply_function_by_name, vm::Vm,
        },
        test_support::test_config,
        util::process_builder::{ProcessCreator, ProcessInitializer},
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
            .process_create_from_code("/maybe_interactive.c", maybe_interactive)
            .await
            .unwrap();

        let master_proc = vm
            .process_initialize_from_code("master.c", master)
            .await
            .unwrap();

        let prototype = master_proc
            .context
            .object_space()
            .lookup("/maybe_interactive")
            .unwrap()
            .clone();

        assert!(prototype.flags.test(ObjectFlags::CommandsEnabled));

        apply_function_by_name(
            "disabler",
            &[],
            master_proc.context.process,
            vm.new_task_template(),
            None,
        )
        .await
        .unwrap()
        .unwrap();

        assert!(!prototype.flags.test(ObjectFlags::CommandsEnabled));
    }
}
