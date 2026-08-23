use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{
    efun, efun::efun_context::EfunContext, lpc_ref::LpcRef, object_flags::ObjectFlags,
};

/// `living`, an efun that returns whether an object (the caller by default)
/// has commands enabled.
pub async fn living<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg_ref = context.resolve_local_register(1 as RegisterSize);

    let result = efun::arg_or_this_object(arg_ref, context)
        .is_some_and(|proc| proc.flags.test(ObjectFlags::CommandsEnabled));

    context.return_efun_result(LpcRef::from(result));

    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{CommittedReader, lpc_int::LpcInt, lpc_ref::LpcRef, vm::Vm},
        test_support::{run_prog, test_config},
    };

    #[tokio::test]
    async fn no_arg_is_this_object() {
        let code = indoc! { r#"
            int before = living();
            int after = enable_then_living();

            int enable_then_living() {
                enable_commands();
                return living();
            }
        "# };

        let task = run_prog(code).await;
        let gs = &task.context.global_state;
        let proc = task.context.process();

        assert_eq!(gs.committed_global(proc, 0), LpcRef::from(0));
        assert_eq!(gs.committed_global(proc, 1), LpcRef::from(1));
    }

    #[tokio::test]
    async fn test_living() {
        let master = indoc! { r#"
            int create() {
                "/maybe_living"->set_living(1);
                object ob = find_object("/maybe_living");
                
                return living(ob);
            }
        "# };

        let maybe_living = indoc! { r#"
            void set_living() {
                enable_commands();
            }
        "# };

        let vm = Vm::new(test_config());
        let _maybe_living_proc = vm
            .create_process_from_code("/maybe_living.c", maybe_living)
            .await
            .unwrap();

        let master_proc = vm
            .initialize_process_from_code("master.c", master)
            .await
            .unwrap();

        let result = master_proc.result().unwrap();
        assert_eq!(result, LpcRef::Int(LpcInt(1)));
    }
}
