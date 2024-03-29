use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// `remove_call_out`, an efun for removing a call out.
/// This will cancel both upcoming and repeating call outs.
pub async fn remove_call_out<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let LpcRef::Int(idx) = context.resolve_local_register(1  as RegisterSize) else {
        return Err(context.runtime_bug("non-int call out ID sent to `remove_call_out`"));
    };

    if idx.0 < 0 {
        return Err(context.runtime_error(format!(
            "invalid call out ID `{idx}` sent to `remove_call_out`"
        )));
    }

    let removed = {
        let mut call_outs = context.call_outs().write();
        call_outs.remove(idx.0 as usize)
    };

    let ret = removed
        .map(|call_out| {
            call_out
                .time_remaining()
                .map(|duration| duration.num_milliseconds())
                .unwrap_or(0)
        })
        .unwrap_or(-1);

    let result = LpcRef::Int(ret.into());
    context.return_efun_result(result);

    Ok(())
}

#[cfg(test)]
mod tests {

    use std::sync::Arc;

    use crate::{
        interpreter::{
            task::initialize_program::InitializeProgramBuilder, vm::global_state::GlobalState,
        },
        test_support::compile_prog,
    };

    #[tokio::test]
    async fn test_removes_task() {
        let code = r##"
            void create() {
                int id = call_out(call_out_test, 100);

                remove_call_out(id);
            }

            void call_out_test() {
                dump("foobar");
            }
        "##;

        let (tx, _rx) = tokio::sync::mpsc::channel(128);
        let (program, config, _) = compile_prog(code).await;
        let global_state = Arc::new(GlobalState::new(config, tx));
        let result = InitializeProgramBuilder::<10>::default()
            .program(program)
            .global_state(global_state.clone())
            .build()
            .await;

        assert!(result.is_ok());
        assert!(global_state.call_outs.read().is_empty());
    }
}
