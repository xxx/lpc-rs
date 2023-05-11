use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, object_flags::ObjectFlags};

/// `enable_commands`, an efun that enables an object to interact with the game world.
pub async fn enable_commands<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let proc = &context.frame().process;

    proc.flags.set(ObjectFlags::CommandsEnabled);

    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{object_flags::ObjectFlags, vm::Vm},
        test_support::test_config,
        util::process_builder::{ProcessCreator, ProcessInitializer},
    };

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

        assert!(prototype.flags.test(ObjectFlags::CommandsEnabled));
        assert!(!clone.flags.test(ObjectFlags::CommandsEnabled));
    }
}
