use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{
    CATCH_TELL,
    apply::apply_nested,
    efun::{efun_context::EfunContext, write::record_output_effect},
    lpc_ref::LpcRef,
};

pub async fn tell_object<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let string_ref = context.resolve_local_register(2 as RegisterSize).clone();
    let ob_ref = context.resolve_local_register(1 as RegisterSize);
    let proc = if let Some(path) = ob_ref.as_str() {
        let path = context.in_game_path(path);
        Some(context.load_object(&path).await?)
    } else {
        ob_ref.live_object(context.txn())
    };

    let delivered = match proc {
        Some(proc) if proc.commands_enabled(context.txn()) => {
            match proc.program.unmangled_functions.get(CATCH_TELL).cloned() {
                Some(catch_tell) => {
                    apply_nested(
                        context.task_context(),
                        &proc,
                        catch_tell,
                        std::slice::from_ref(&string_ref),
                    )
                    .await?;
                    true
                }
                None => false,
            }
        }
        _ => false,
    };

    if delivered {
        context.return_efun_result(LpcRef::from(1));
    } else {
        let msg = string_ref.with_string(|s| s.to_string())?;
        record_output_effect(context, msg);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use itertools::Itertools;

    use crate::{
        interpreter::{CommittedReader, vm::Vm},
        test_support::test_config,
    };

    #[tokio::test]
    async fn catch_tell_sees_the_callers_command_giver() {
        let target = indoc! { r#"
            object giver;

            void create() {
                enable_commands();
            }

            void catch_tell(string message) {
                giver = this_player();
            }
        "# };

        let master = indoc! { r#"
            void create() {
                set_this_player(this_object());
                tell_object("/target", "hi");
            }
        "# };

        let vm = Vm::new(test_config());
        let target = vm
            .initialize_process_from_code("/target.c", target)
            .await
            .unwrap()
            .context
            .process;
        let master_proc = vm
            .initialize_process_from_code("master.c", master)
            .await
            .unwrap()
            .context
            .process;

        assert_eq!(
            vm.global_state.committed_global(&target, 0u16),
            crate::interpreter::lpc_ref::LpcRef::from(std::sync::Arc::downgrade(&master_proc))
        );
    }

    #[tokio::test]
    async fn test_tell_object() {
        let master = indoc! { r#"
            void create() {
                object ob = clone_object("/enabled");
                tell_object(ob, "i herd");
                tell_object("/enabled#0", "u liek mudkips?");

                ob = clone_object("/disabled");
                tell_object(ob, "i don't herd");
            }
        "# };

        let enabled = indoc! { r#"
            inherit "/hears";

            void create() {
                enable_commands();
            }
        "# };

        let disabled = indoc! { r#"
            inherit "/hears";
        "# };

        let vm = Vm::new(test_config());
        let _enabled_proc = vm
            .create_process_from_code("/enabled.c", enabled)
            .await
            .unwrap();
        let _disabled_proc = vm
            .create_process_from_code("/disabled.c", disabled)
            .await
            .unwrap();

        let master_proc = vm
            .initialize_process_from_code("master.c", master)
            .await
            .unwrap();

        let space = master_proc.context.object_space();

        let enabled = space.lookup("/enabled#0").unwrap();

        let g_enabled = vm.global_state.committed_global(&enabled, 0u16);
        let crate::interpreter::lpc_ref::LpcRef::Array(cell) = g_enabled else {
            panic!("global holds an array cell, actually {g_enabled:?}");
        };
        let arr = vm
            .global_state
            .committed_array(cell.id)
            .expect("array payload committed");
        assert_eq!(
            &arr.iter().map(|s| s.to_string()).collect_vec(),
            &["i herd", "u liek mudkips?"]
        );

        let disabled = space.lookup("/disabled#1").unwrap();
        let g_disabled = vm.global_state.committed_global(&disabled, 0u16);
        let crate::interpreter::lpc_ref::LpcRef::Array(cell) = g_disabled else {
            panic!("global holds an array cell, actually {g_disabled:?}");
        };
        let arr = vm
            .global_state
            .committed_array(cell.id)
            .expect("array payload committed");
        assert!(arr.is_empty());
    }
}
