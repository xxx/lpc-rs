use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::efun_context::EfunContext, into_lpc_ref::IntoLpcRef, lpc_ref::LpcRef,
};

/// `papplyv`, an efun to partially apply a function to arguments taken from an array
pub async fn papplyv<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let LpcRef::Function(func) = context.resolve_local_register(1_usize) else {
        return Err(context.runtime_error("non-function argument sent to `papplyv`"));
    };

    let LpcRef::Array(arr) = context.resolve_local_register(2_usize) else {
        return Err(context.runtime_error("non-array argument sent to `papplyv`"));
    };

    // TODO: this clone is unnecessarily heavy
    let mut ptr = func.read().clone_with_new_id();
    ptr.partially_apply(&arr.read());

    let result = ptr.into_lpc_ref(context.memory());

    context.return_efun_result(result);

    Ok(())
}

#[cfg(test)]
mod tests {

    use std::sync::Arc;

    use lpc_rs_utils::config::Config;
    use parking_lot::RwLock;

    use super::*;
    use crate::{
        interpreter::{
            call_outs::CallOuts, gc::gc_bank::GcBank, memory::Memory, object_space::ObjectSpace,
            task::Task,
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
        let (program, _, _) = compile_prog(code);
        let call_outs = Arc::new(RwLock::new(CallOuts::new(tx.clone())));
        let result = Task::<5>::initialize_program(
            program,
            Config::default(),
            ObjectSpace::default(),
            Memory::default(),
            RwLock::new(GcBank::default()),
            call_outs,
            tx,
        )
        .await;

        let b = result.unwrap();
        let r = b.result().unwrap();

        let LpcRef::Function(func) = r else {
            panic!("expected function ref");
        };

        let func = func.read();

        assert_eq!(func.name(), "dump");

        assert_eq!(
            func.partial_args
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
