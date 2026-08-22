use lpc_rs_core::{RegisterSize, lpc_path::LpcPath};
use lpc_rs_errors::Result;

use crate::interpreter::{
    CATCH_TELL,
    efun::{efun_context::EfunContext, write::record_output_effect},
    lpc_ref::LpcRef,
    object_flags::ObjectFlags,
    task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
};

/// `tell_object`, an efun for sending a message to a specific object
pub async fn tell_object<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let should_log = do_tell(context).await?;

    if should_log {
        let string_ref = context.resolve_local_register(2 as RegisterSize);
        let msg = string_ref.with_string(|s| s.to_string())?;
        record_output_effect(context, msg);
    }

    Ok(())
}

async fn do_tell<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<bool> {
    let string_ref = context.resolve_local_register(2 as RegisterSize);
    let ob_ref = context.resolve_local_register(1 as RegisterSize);
    let proc = match ob_ref {
        LpcRef::Float(_)
        | LpcRef::Int(_)
        | LpcRef::Array(_)
        | LpcRef::Mapping(_)
        | LpcRef::Function(_) => {
            return Ok(true);
        }
        LpcRef::String(_) => {
            let path = ob_ref.with_string(|str| {
                LpcPath::new_in_game(str, context.in_game_cwd(), &*context.config().lib_dir)
            })?;

            context.load_object(&path).await?
        }
        LpcRef::Object(proc) => {
            let Some(proc) = proc.upgrade() else {
                return Ok(true);
            };

            proc
        }
    };

    if !proc.flags.test(ObjectFlags::CommandsEnabled) {
        return Ok(true);
    }

    match apply_function_by_name(
        CATCH_TELL,
        std::slice::from_ref(string_ref),
        proc.clone(),
        TaskTemplate::from(&*context),
        Some(context.config().max_execution_time),
    )
    .await
    {
        Some(Ok(_)) => {
            context.return_efun_result(LpcRef::from(1));
            Ok(false)
        }
        Some(Err(e)) => Err(e),
        None => Ok(true),
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use itertools::Itertools;
    use lpc_rs_errors::lazy_files::FILE_CACHE;

    use crate::{
        interpreter::{CommittedReader, vm::Vm},
        test_support::test_config,
        util::process_builder::ProcessCreator,
    };

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

        let hears = indoc! { r#"
            string *i_herd = ({});

            void catch_tell(string message) {
                i_herd += ({ message });
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

        // The inherited file just needs to be there.
        let path = format!("{}/hears.c", test_config().lib_dir);
        FILE_CACHE.write().add_eager(path, hears);

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
