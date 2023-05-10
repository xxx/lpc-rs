use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::efun_context::EfunContext, into_lpc_ref::IntoLpcRef, lpc_ref::LpcRef,
};

/// `papplyv`, an efun to partially apply a function to arguments taken from an array
pub async fn papplyv<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let LpcRef::Function(func) = context.resolve_local_register(1 as RegisterSize) else {
        return Err(context.runtime_error("non-function argument sent to `papplyv`"));
    };

    let LpcRef::Array(arr) = context.resolve_local_register(2 as RegisterSize) else {
        return Err(context.runtime_error("non-array argument sent to `papplyv`"));
    };

    let ptr = func.clone_with_new_id();
    ptr.partially_apply(&arr.read());

    let result = ptr.into_lpc_ref(context.memory());

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

        assert_eq!(
            func.partial_args
                .read()
                .iter()
                .map(|a| a.as_ref().unwrap().to_string())
                .collect::<Vec<_>>(),
            vec!["foo", "bar"]
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
        );
    }
}
