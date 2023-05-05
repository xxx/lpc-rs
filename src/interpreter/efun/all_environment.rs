use std::sync::{Arc, Weak};
use if_chain::if_chain;
use lpc_rs_core::{RegisterSize};
use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::efun_context::EfunContext,
    into_lpc_ref::IntoLpcRef,
    lpc_ref::{LpcRef, NULL},
};
use crate::interpreter::lpc_array::LpcArray;
use crate::interpreter::lpc_int::LpcInt;
use crate::interpreter::process::Process;

/// `all_environment`, an efun for returning all wrapping environments of an object.
pub async fn all_environment<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg_ref = context.resolve_local_register(1 as RegisterSize);

    let Some(current_env) = determine_proc(&arg_ref, context) else {
        let result = LpcArray::default().into_lpc_ref(context.memory());
        context.return_efun_result(result);
        return Ok(());
    };

    let iter = Process::all_environment(current_env)
        .map(|e| Arc::downgrade(&e).into_lpc_ref(context.memory()));
    let result = iter.collect::<LpcArray>().into_lpc_ref(context.memory());
    context.return_efun_result(result);

    Ok(())
}

fn determine_proc<const N: usize>(arg_ref: &LpcRef, context: &EfunContext<'_, N>) -> Option<Arc<Process>> {
    match arg_ref {
        LpcRef::Int(LpcInt(0)) => {
            Some(context.frame().process.clone())
        }
        LpcRef::Object(proc) => {
            proc.upgrade()
        }
        LpcRef::Float(_)
        | LpcRef::Int(_)
        | LpcRef::String(_)
        | LpcRef::Array(_)
        | LpcRef::Mapping(_)
        | LpcRef::Function(_) => None,
    }
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

        let master = indoc! { r#"
            object *create() {
                "/foo"->do_moves();
                return all_environment(find_object("/foo"));
            }
        "# };

        let vm = Vm::new(test_config());

        let foo_proc = vm.process_create_from_code("/foo.c", foo).await.unwrap();
        let outer_proc = vm.process_initialize_from_code("/outer.c", "").await.unwrap();
        let inner_proc = vm.process_initialize_from_code("/inner.c", inner).await.unwrap();
        let innermost_proc = vm.process_initialize_from_code("/innermost.c", innermost).await.unwrap();

        let master_proc = vm
            .process_initialize_from_code("/master.c", master)
            .await
            .unwrap();

        let LpcRef::Array(result) = master_proc.result().unwrap() else {
            panic!("Expected array result");
        };

        let result = result.read();
        assert_eq!(result.as_ref(), &[&*innermost_proc.context.process, &*inner_proc.context.process, &*outer_proc.context.process]);
    }
}