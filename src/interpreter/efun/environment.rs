use if_chain::if_chain;
use lpc_rs_core::{RegisterSize};
use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::efun_context::EfunContext,
    into_lpc_ref::IntoLpcRef,
    lpc_ref::{LpcRef, NULL},
};
use crate::interpreter::lpc_int::LpcInt;

/// `environment`, an efun for returning the environment of an object
pub async fn environment<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg_ref = context.resolve_local_register(1 as RegisterSize);
    let result = match arg_ref {
        LpcRef::Int(LpcInt(0)) => {
            let proc = &context.frame().process;

            if let Some(env) = proc.position.environment.load_full() {
                env.into_lpc_ref(context.memory())
            } else {
                NULL
            }
        }
        LpcRef::Object(proc) => {
            if_chain! {
                if let Some(proc) = proc.upgrade();
                if let Some(env) = proc.position.environment.load_full();
                then {
                    env.into_lpc_ref(context.memory())
                } else {
                    NULL
                }
            }
        }
        LpcRef::Float(_)
        | LpcRef::Int(_)
        | LpcRef::String(_)
        | LpcRef::Array(_)
        | LpcRef::Mapping(_)
        | LpcRef::Function(_) => NULL,
    };

    context.return_efun_result(result);

    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use crate::interpreter::lpc_ref::LpcRef;
    use crate::interpreter::vm::Vm;
    use crate::test_support::test_config;
    use crate::util::process_builder::{ProcessCreator, ProcessInitializer};

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

        let foo_proc = vm.process_create_from_code("/foo.c", foo).await.unwrap();
        let _bar_proc = vm.process_create_from_code("/bar.c", "").await.unwrap();

        let master_proc = vm
            .process_initialize_from_code("/master.c", master)
            .await
            .unwrap();

        let LpcRef::Object(result) = master_proc.result().unwrap() else {
            panic!("Expected object result");
        };

        let result = result.upgrade().unwrap();

        assert_eq!(
            foo_proc
                .position
                .environment
                .swap(None)
                .unwrap()
                .upgrade()
                .unwrap(),
            result
        );
    }
}