use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// `papplyv`, an efun to partially apply a function to arguments taken from an array
pub async fn papplyv<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let LpcRef::Function(func) = context.resolve_local_register(1 as RegisterSize) else {
        return Err(context.runtime_error("non-function argument sent to `papplyv`"));
    };

    let ptr = func.clone_with_new_id();

    let arg_array = context.resolve_local_register(2 as RegisterSize);

    let result = arg_array.with_array(context.txn(), |arr| {
        ptr.partially_apply(arr);

        ptr.into()
    })?;

    context.return_efun_result(result);

    Ok(())
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{
        interpreter::{
            task::initialize_program::InitializeProgramBuilder, vm::global_state::GlobalState,
        },
        test_support::compile_prog,
    };

    #[tokio::test]
    async fn test_papplyv() {
        let code = r##"
            function create() {
                return papplyv(dump, ({ "foo", "bar" }));
            }
        "##;

        let (tx, _rx) = tokio::sync::mpsc::channel(128);
        let (program, config, _) = compile_prog(code).await;
        let global_state = GlobalState::new(config, tx);
        let result = InitializeProgramBuilder::<5>::default()
            .global_state(global_state)
            .program(program)
            .build()
            .await;

        let b = result.unwrap();
        let r = b.result().unwrap();

        let LpcRef::Function(func) = r else {
            panic!("expected function ref");
        };

        assert_eq!(func.name(), "dump");

        func.with_partial_args(|args| {
            assert_eq!(
                args.iter()
                    .map(|a| a.as_ref().unwrap().to_string())
                    .collect::<Vec<_>>(),
                ["foo", "bar"]
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
            );
        });
    }
}
