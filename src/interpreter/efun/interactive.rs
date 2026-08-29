use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{efun, efun::efun_context::EfunContext, lpc_ref::LpcRef};

pub async fn interactive<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg_ref = context.resolve_local_register(1 as RegisterSize);
    let result = efun::arg_or_this_object(arg_ref, context)
        .await?
        .is_some_and(|proc| {
            context
                .txn()
                .with(|t| t.read_connection(proc.connection.id))
                .is_some()
        });

    context.return_efun_result(LpcRef::from(result));

    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{
            lpc_int::LpcInt, lpc_ref::LpcRef, task::Task, task::task_template::TaskTemplate, vm::Vm,
        },
        test_support::{connect, test_config},
    };

    #[tokio::test]
    async fn test_interactive() {
        let master = indoc! { r#"
            int create() {
                return interactive();
            }
        "# };

        let vm = Vm::new(test_config());
        let master_proc = vm
            .create_process_from_code("master.c", master)
            .await
            .unwrap();
        let _connected = connect(&vm, &master_proc).await;

        let task = Task::<16>::initialize_process(
            TaskTemplate::from(vm.global_state.clone()).into_task_context(master_proc),
        )
        .await
        .unwrap();

        let result = task.result().unwrap();
        assert_eq!(result, LpcRef::Int(LpcInt(1)));
    }
}
