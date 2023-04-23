use lpc_rs_errors::Result;
use tracing::trace;

use crate::interpreter::{
    efun::efun_context::EfunContext, into_lpc_ref::IntoLpcRef, lpc_ref::LpcRef,
    lpc_string::LpcString, task::apply_function::apply_function_by_name, CATCH_TELL,
};

/// `write`, an efun for writing to this_player().
pub async fn write<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg_ref = context.resolve_local_register(1_usize);

    let msg = arg_ref.to_string();

    let player_guard = context.this_player().load();
    let Some(this_player) = &*player_guard else {
        // TODO: write the message to the debug log;
        context.return_efun_result(LpcRef::from(0));
        return Ok(());
    };

    let mut ctx = context.clone_task_context();
    ctx.process = this_player.clone();

    let result = apply_function_by_name(
        CATCH_TELL,
        &[LpcString::from(msg).into_lpc_ref(context.memory())],
        this_player.clone(),
        ctx,
    )
    .await;

    if let Some(Ok(_)) = result {
        context.return_efun_result(LpcRef::from(1));
        return Ok(());
    }

    // TODO: emit to debug log
    trace!(%this_player, "write: failed to write to this_player(): {:?}", result);

    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{interpreter::vm::Vm, test_support::test_config};

    #[tokio::test]
    async fn test_write_calls_catch_tell() {
        let code = r##"
            string name = "my name is ";

            void create() {
                set_this_player(this_object()); // needed for testing
                write("foobar");
            }

            void catch_tell(string s) {
                name += s;
            }
        "##;

        let mut vm = Vm::new(test_config());
        let result = vm
            .initialize_string(code, "test_write.c")
            .await
            .map_err(|e| {
                e.emit_diagnostics();
                e
            })
            .unwrap();

        assert_eq!(
            result
                .process
                .global_variable_values()
                .get("name")
                .unwrap()
                .to_string(),
            "my name is foobar"
        );
    }
}
