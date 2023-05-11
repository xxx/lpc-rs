use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::efun_context::EfunContext, lpc_int::LpcInt, lpc_ref::LpcRef, object_flags::ObjectFlags,
};

/// `living`, an efun that returns whether an object is alive, and can interact with the world.
pub async fn living<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg_ref = context.resolve_local_register(1 as RegisterSize);

    let result = match arg_ref {
        LpcRef::Int(LpcInt(0)) => {
            let proc = &context.frame().process;

            LpcRef::from(proc.connection.load().is_some())
        }
        LpcRef::Object(proc) => {
            if let Some(proc) = proc.upgrade() {
                LpcRef::from(proc.flags.test(ObjectFlags::CommandsEnabled))
            } else {
                LpcRef::from(false)
            }
        }
        LpcRef::Float(_)
        | LpcRef::Int(_)
        | LpcRef::String(_)
        | LpcRef::Array(_)
        | LpcRef::Mapping(_)
        | LpcRef::Function(_) => LpcRef::from(false),
    };

    context.return_efun_result(result);

    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{lpc_int::LpcInt, lpc_ref::LpcRef, vm::Vm},
        test_support::test_config,
        util::process_builder::{ProcessCreator, ProcessInitializer},
    };

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
        assert_eq!(result, &LpcRef::Int(LpcInt(1)));
    }
}
