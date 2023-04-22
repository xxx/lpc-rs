use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// `destruct`, an efun for deleting objects from the [`ObjectSpace`]
pub async fn destruct<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let lpc_ref = context.resolve_local_register(1_usize);
    match lpc_ref {
        LpcRef::Float(_)
        | LpcRef::Int(_)
        | LpcRef::String(_)
        | LpcRef::Mapping(_)
        | LpcRef::Function(_) => {}
        LpcRef::Array(arr) => {
            let arr = arr.read();

            for x in arr.iter() {
                let LpcRef::Object(proc) = x else {
                    continue;
                };

                if let Some(proc) = proc.upgrade() {
                    context.remove_process(proc);
                } // else it's already destructed
            }
        }
        LpcRef::Object(proc) => {
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

    use crate::{
        interpreter::task::initialize_program::InitializeProgramBuilder, test_support::compile_prog,
    };

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
        let (program, _, _) = compile_prog(code).await;
        let result = InitializeProgramBuilder::<5>::default()
            .program(program)
            .tx(tx)
            .build()
            .await
            .unwrap();

        assert!(result.context.object_space.is_empty());
    }
}
