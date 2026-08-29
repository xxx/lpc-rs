use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{
    CATCH_TELL, apply::apply_nested, efun::efun_context::EfunContext, lpc_ref::LpcRef,
    lpc_string::LpcString, stm::Effect,
};

/// `write`, an efun for writing to this_player().
pub async fn write<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg_ref = context.resolve_local_register(1 as RegisterSize);

    let msg = arg_ref.to_string();

    apply_catch_tell(msg, context).await?;

    Ok(())
}

/// Record a physical write for delivery after this task's transaction
/// commits, instead of sending it now. Delivery (or its cancellation on a
/// rejected attempt) is handled by the retry loop; the message is already
/// materialized, so the effect never observes end-of-transaction state.
pub(crate) fn record_output_effect<const N: usize>(context: &EfunContext<'_, N>, msg: String) {
    context.record_effect(Effect::DebugLog(msg));
}

/// `catch_tell` on `this_player` with `msg`, else the debug log; the efun
/// result is 1 when delivered.
pub(crate) async fn apply_catch_tell<const N: usize>(
    msg: String,
    context: &mut EfunContext<'_, N>,
) -> Result<()> {
    let player_guard = context.this_player().load();
    let Some(this_player) = &*player_guard else {
        record_output_effect(context, msg);
        return Ok(());
    };

    let Some(catch_tell) = this_player.program.unmangled_functions.get(CATCH_TELL) else {
        record_output_effect(context, msg);
        return Ok(());
    };

    apply_nested(
        context.task_context(),
        this_player,
        catch_tell.clone(),
        &[LpcString::from(&msg).into()],
    )
    .await?;
    context.return_efun_result(LpcRef::from(1));
    Ok(())
}

#[cfg(test)]
mod tests {
    use lpc_rs_core::register::RegisterVariant;

    use crate::{
        interpreter::{CommittedReader, vm::Vm},
        test_support::test_config,
    };

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

        let vm = Vm::new(test_config());
        let result = vm
            .initialize_string(code, "test_write.c")
            .await
            .inspect_err(|e| {
                e.emit_diagnostics();
            })
            .unwrap();

        let name = result.process.program.global_variables.get("name").unwrap();
        let RegisterVariant::Global(reg) = name.location.unwrap() else {
            panic!("name is not a global");
        };
        assert_eq!(
            vm.global_state
                .committed_global(&result.process, reg.index())
                .to_string(),
            "my name is foobar"
        );
    }
}
