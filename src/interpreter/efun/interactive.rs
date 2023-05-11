use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_int::LpcInt, lpc_ref::LpcRef};

/// `interactive`, an efun that returns whether an object is actively controlled by a person.
pub async fn interactive<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg_ref = context.resolve_local_register(1 as RegisterSize);

    let result = match arg_ref {
        LpcRef::Int(LpcInt(0)) => {
            let proc = &context.frame().process;

            LpcRef::from(proc.connection.load().is_some())
        }
        LpcRef::Object(proc) => {
            if let Some(proc) = proc.upgrade() {
                LpcRef::from(proc.connection.load().is_some())
            } else {
                LpcRef::from(false)
            }
        }
        LpcRef::Float(_)
        | LpcRef::Int(LpcInt(_))
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
    use std::{net::ToSocketAddrs, sync::Arc};

    use arc_swap::ArcSwapAny;
    use indoc::indoc;

    use crate::{
        interpreter::{
            lpc_int::LpcInt,
            lpc_ref::LpcRef,
            task::{into_task_context::IntoTaskContext, Task},
            vm::Vm,
        },
        telnet::connection::Connection,
        test_support::test_config,
        util::process_builder::ProcessCreator,
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
        let (tx, _rx) = tokio::sync::mpsc::channel(1);
        let (broker_tx, _broker_rx) = flume::unbounded();

        let connection = Connection {
            address: "127.0.0.1:23123".to_socket_addrs().unwrap().next().unwrap(),
            process: ArcSwapAny::from(Some(master_proc.clone())),
            tx,
            broker_tx,
            input_to: Default::default(),
        };

        master_proc.connection.store(Some(Arc::new(connection)));

        let task =
            Task::<16>::initialize_process(vm.new_task_template().into_task_context(master_proc))
                .await
                .unwrap();

        let result = task.result().unwrap();
        assert_eq!(result, &LpcRef::Int(LpcInt(1)));
    }
}
