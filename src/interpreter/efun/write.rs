use lpc_rs_core::RegisterSize;
use lpc_rs_errors::{lpc_error, Result};

use crate::interpreter::{
    efun::efun_context::EfunContext, into_lpc_ref::IntoLpcRef, lpc_ref::LpcRef,
    lpc_string::LpcString, task::apply_function::apply_function_by_name, CATCH_TELL,
};

/// `write`, an efun for writing to this_player().
pub async fn write<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg_ref = context.resolve_local_register(1 as RegisterSize);

    let msg = arg_ref.to_string();

    apply_catch_tell(msg, context).await?;

    Ok(())
}

/// A convenience helper to apply catch_tell to the this_player in a context.
pub async fn apply_catch_tell<const N: usize>(
    msg: String,
    context: &mut EfunContext<'_, N>,
) -> Result<()> {
    let player_guard = context.this_player().load();
    let Some(this_player) = &*player_guard else {
        context.config().debug_log(msg.to_string()).await;
        return Ok(());
    };

    let ctx = context
        .task_context_builder()
        .process(this_player.clone())
        .build()
        .unwrap();

    let max_execution_time = context.config().max_execution_time;
    let result = apply_function_by_name(
        CATCH_TELL,
        &[LpcString::from(&msg).into_lpc_ref(context.memory())],
        this_player.clone(),
        ctx,
        Some(max_execution_time),
    )
    .await;

    match result {
        Some(Ok(_)) => {
            context.return_efun_result(LpcRef::from(1));
            Ok(())
        }
        Some(Err(e)) => Err(lpc_error!(
            "write: failed to write to this_player(): {:?}",
            e
        )),
        None => {
            context.config().debug_log(msg).await;
            Ok(())
        }
    }
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
