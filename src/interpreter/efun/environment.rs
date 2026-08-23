use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{efun, efun::efun_context::EfunContext, lpc_ref::NULL};

pub async fn environment<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg_ref = context.resolve_local_register(1 as RegisterSize);
    let result = efun::arg_or_this_object(arg_ref, context)
        .and_then(|object| {
            context
                .txn()
                .with(|t| t.read(object.position.environment.id))
        })
        .unwrap_or(NULL);

    context.return_efun_result(result);

    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{CommittedReader, lpc_ref::LpcRef, vm::Vm},
        test_support::test_config,
    };

    #[tokio::test]
    async fn test_environment() {
        let foo = indoc! { r#"
            void do_moves() {
                move_object("/bar");
            }
        "# };

        let master = indoc! { r#"
            object create() {
                "/foo"->do_moves();
                return environment(find_object("/foo"));
            }
        "# };

        let vm = Vm::new(test_config());

        let foo_proc = vm.create_process_from_code("/foo.c", foo).await.unwrap();
        let _bar_proc = vm.create_process_from_code("/bar.c", "").await.unwrap();

        let master_proc = vm
            .initialize_process_from_code("/master.c", master)
            .await
            .unwrap();

        let LpcRef::Object(result) = master_proc.result().unwrap() else {
            panic!("Expected object result");
        };

        let result = result.upgrade().unwrap();

        assert_eq!(
            vm.global_state.committed_environment(&foo_proc),
            Some(result)
        );
    }
}
