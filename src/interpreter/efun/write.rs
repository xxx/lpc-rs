use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{
    apply::deliver, efun::efun_context::EfunContext, lpc_ref::LpcRef, stm::Effect,
};

/// `write`, an efun for writing to this_player().
pub async fn write<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let msg = context
        .resolve_local_register(1 as RegisterSize)
        .to_string();
    let received = tell_this_player(context, &msg).await?;
    context.return_efun_result(LpcRef::from(received));
    Ok(())
}

/// `msg` to `this_player` through [`deliver`]; the debug log, not received,
/// when there is none.
pub(crate) async fn tell_this_player<const N: usize>(
    context: &EfunContext<'_, N>,
    msg: &str,
) -> Result<bool> {
    match context.this_player().load_full() {
        Some(player) => deliver(context.task_context(), &player, None, msg).await,
        None => {
            context.record_effect(Effect::DebugLog(msg.to_owned()));
            Ok(false)
        }
    }
}

#[cfg(test)]
mod tests {
    use lpc_rs_core::register::RegisterVariant;

    use std::{net::ToSocketAddrs, sync::Arc};

    use arc_swap::ArcSwapAny;
    use indoc::indoc;

    use crate::{
        interpreter::{CommittedReader, lpc_ref::LpcRef, vm::Vm},
        telnet::{connection::Connection, ops::ConnectionOp},
        test_support::test_config,
    };

    #[tokio::test]
    async fn test_write_calls_catch_tell() {
        let code = r##"
            string name = "my name is ";

            void create() {
                set_this_player(this_object()); // needed for testing
                write("foobar");
            }

            void catch_tell(string s) {
                name += s;
            }
        "##;

        let vm = Vm::new(test_config());
        let result = vm
            .initialize_string(code, "test_write.c")
            .await
            .inspect_err(|e| {
                e.emit_diagnostics();
            })
            .unwrap();

        let name = result.process.program.global_variables.get("name").unwrap();
        let RegisterVariant::Global(reg) = name.location.unwrap() else {
            panic!("name is not a global");
        };
        assert_eq!(
            vm.global_state
                .committed_global(&result.process, reg.index())
                .to_string(),
            "my name is foobar"
        );
    }

    #[tokio::test]
    async fn a_player_without_catch_tell_is_written_to_on_its_connection() {
        let (tx, mut rx) = tokio::sync::mpsc::channel(4);
        let (broker_tx, _broker_rx) = flume::unbounded();
        let vm = Vm::new(test_config());
        let player = vm.create_process_from_code("/player.c", "").await.unwrap();
        let connection = Connection {
            address: "127.0.0.1:23123".to_socket_addrs().unwrap().next().unwrap(),
            process: ArcSwapAny::from(Some(player.clone())),
            tx,
            broker_tx,
            input_to: Default::default(),
        };
        vm.global_state
            .takeover(Arc::new(connection), player.clone())
            .await;

        let main = indoc! { r#"
            int r;
            void create() { set_this_player(find_object("/player")); r = write("hi"); }
        "# };
        let main = vm
            .initialize_process_from_code("/main.c", main)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            vm.global_state.committed_global(&main, 0u16),
            LpcRef::from(1)
        );
        assert_eq!(rx.try_recv(), Ok(ConnectionOp::SendMessage("hi".into())));
    }
}
