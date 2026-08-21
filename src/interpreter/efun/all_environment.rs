use std::sync::Arc;

use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{
    efun, efun::efun_context::EfunContext, lpc_array::LpcArray, lpc_ref::LpcRef, process::Process,
};

/// `all_environment`, an efun for returning all wrapping environments of an object.
pub async fn all_environment<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg_ref = context.resolve_local_register(1 as RegisterSize);

    let Some(current_env) = efun::arg_or_this_object(arg_ref, context) else {
        let result = LpcRef::Array(context.txn().with(|t| t.mint_array(LpcArray::default())));
        context.return_efun_result(result);
        return Ok(());
    };

    let entries: Vec<LpcRef> = Process::all_environment(context.txn().clone(), current_env)
        .map(|e| LpcRef::from(Arc::downgrade(&e)))
        .collect();
    let result = context.txn().with(|t| {
        let array: LpcArray = entries.into_iter().collect();
        LpcRef::Array(t.mint_array(array))
    });
    context.return_efun_result(result);

    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::vm::Vm,
        test_support::test_config,
        util::process_builder::{ProcessCreator, ProcessInitializer},
    };

    #[tokio::test]
    async fn test_all_environment() {
        let inner = indoc! { r#"
            void create() {
                move_object("/outer");
            }
        "# };
        let innermost = indoc! { r#"
            void create() {
                move_object("/inner");
            }
        "# };

        let foo = indoc! { r#"
            void do_moves() {
                move_object("/innermost");
            }
        "# };

        let outer = indoc! { r#"
            void create() {
                move_object("/foo"); // creates a loop, to ensure we handle them.
            }
        "# };

        let master = indoc! { r#"
            object *create() {
                "/foo"->do_moves();
                return all_environment(find_object("/foo"));
            }
        "# };

        let vm = Vm::new(test_config());

        let foo_proc = vm.create_process_from_code("/foo.c", foo).await.unwrap();
        let outer_proc = vm
            .initialize_process_from_code("/outer.c", outer)
            .await
            .unwrap();
        let inner_proc = vm
            .initialize_process_from_code("/inner.c", inner)
            .await
            .unwrap();
        let innermost_proc = vm
            .initialize_process_from_code("/innermost.c", innermost)
            .await
            .unwrap();

        let master_proc = vm
            .initialize_process_from_code("/master.c", master)
            .await
            .unwrap();

        let _ = master_proc
            .result()
            .unwrap()
            .with_array(&master_proc.txn, |result| {
                assert_eq!(
                    result.as_ref(),
                    &[
                        &*innermost_proc.context.process,
                        &*inner_proc.context.process,
                        &*outer_proc.context.process,
                        &*foo_proc,
                    ]
                );
            });
    }
}
