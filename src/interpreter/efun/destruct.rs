use lpc_rs_errors::{LpcError, Result};

use crate::{interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef, lpc_value::LpcValue}, try_extract_value};


/// `destruct`, an efun for deleting objects from the [`ObjectSpace`]
pub async fn destruct<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let lpc_ref = context.resolve_local_register(1_usize);
    match lpc_ref {
        LpcRef::Float(_)
        | LpcRef::Int(_)
        | LpcRef::String(_)
        | LpcRef::Mapping(_)
        | LpcRef::Function(_) => {},
        LpcRef::Array(arr) => {
            let arr = arr.read();
            let LpcValue::Array(arr) = &*arr else {
                return Err(context.runtime_error(format!("destruct() called on non-array: {:?}", arr)));
            };

            for x in arr.iter() {
                let LpcRef::Object(ob) = x else {
                    continue;
                };
                let LpcValue::Object(proc) = &*ob.read() else {
                    continue;
                };

                if let Some(proc) = proc.upgrade() {
                    context.remove_process(proc);
                } // else it's already destructed
            }
        }
        LpcRef::Object(x) => {
            let b = x.read();
            let proc = try_extract_value!(*b, LpcValue::Object);

            if let Some(proc) = proc.upgrade() {
                context.remove_process(proc);
            } // else it's already destructed
        }
    }

    // destruct() returns void, so no need to set up a return value

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;
    use parking_lot::RwLock;
    use lpc_rs_utils::config::Config;
    use crate::interpreter::call_outs::CallOuts;
    use crate::interpreter::gc::gc_bank::GcBank;
    use crate::interpreter::memory::Memory;
    use crate::interpreter::object_space::ObjectSpace;
    use crate::interpreter::task::Task;
    use crate::test_support::compile_prog;

    #[tokio::test]
    async fn test_destruct() {
        let code = r##"
            function create() {
                // won't delete the object immediately, but will delete it
                // after the current Task finishes
                destruct(this_object());
            }
        "##;

        let (tx, _rx) = tokio::sync::mpsc::channel(128);
        let (program, _, _) = compile_prog(code);
        let call_outs = Arc::new(RwLock::new(CallOuts::new(tx.clone())));
        let result = Task::<5>::initialize_program(
            program,
            Config::default(),
            RwLock::new(ObjectSpace::default()),
            Memory::default(),
            RwLock::new(GcBank::default()),
            call_outs,
            tx,
        )
            .await
            .unwrap();

        assert!(result.context.object_space.read().is_empty());
    }
}
