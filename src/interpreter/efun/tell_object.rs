use lpc_rs_core::{lpc_path::LpcPath, RegisterSize};
use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::efun_context::EfunContext,
    lpc_ref::LpcRef,
    object_flags::ObjectFlags,
    task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
    CATCH_TELL,
};

/// `tell_object`, an efun for sending a message to a specific object
pub async fn tell_object<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let should_log = do_tell(context).await?;

    if should_log {
        let string_ref = context.resolve_local_register(2 as RegisterSize);
        let LpcRef::String(lpc_string) = string_ref else {
            return Err(context.runtime_error("expected string as second argument to tell_object"));
        };
        let msg = lpc_string.read().to_string();
        context.config().debug_log(msg).await;
    }

    Ok(())
}

async fn do_tell<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<bool> {
    let string_ref = context.resolve_local_register(2 as RegisterSize);
    let LpcRef::String(_lpc_string) = string_ref else {
        return Err(context.runtime_error("expected string as second argument to tell_object"));
    };

    let ob_ref = context.resolve_local_register(1 as RegisterSize);
    let proc = match ob_ref {
        LpcRef::Float(_)
        | LpcRef::Int(_)
        | LpcRef::Array(_)
        | LpcRef::Mapping(_)
        | LpcRef::Function(_) => {
            return Ok(true);
        }
        LpcRef::String(path) => {
            let path = {
                let string = path.read();
                LpcPath::new_in_game(&*string, context.in_game_cwd(), &*context.config().lib_dir)
            };

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
        &[string_ref.clone()],
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

    use super::*;
    use crate::{
        interpreter::{
            into_lpc_ref::IntoLpcRef, lpc_array::LpcArray, lpc_string::LpcString, vm::Vm,
        },
        test_support::test_config,
        util::process_builder::{ProcessCreator, ProcessInitializer},
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
        let _expected = LpcArray::from(
            [
                LpcString::from("i herd").into_lpc_ref(&vm.global_state.memory),
                LpcString::from("u liek mudkips?").into_lpc_ref(&vm.global_state.memory),
            ]
            .as_slice(),
        )
        .into_lpc_ref(&vm.global_state.memory);
        let LpcRef::Array(enabled_i_herd) = enabled.globals.read()[0].clone() else {
            panic!("expected array");
        };
        assert_eq!(
            &enabled_i_herd
                .read()
                .iter()
                .map(|s| s.to_string())
                .collect_vec(),
            &["i herd", "u liek mudkips?"]
        );

        let disabled = space.lookup("/disabled#1").unwrap();
        let disabled_i_herd = disabled.globals.read()[0].clone();
        let LpcRef::Array(arr) = disabled_i_herd else {
            panic!("expected array");
        };
        assert!(arr.read().is_empty());
    }
}
