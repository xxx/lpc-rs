use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::{
    interpreter::efun::{self, efun_context::EfunContext},
    telnet::ops::ConnectionOp,
};

/// `send_gmcp`, an efun sending one GMCP message to an object's connection;
/// the session drops it while the client has GMCP off.
pub fn send_gmcp<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let package = context
        .resolve_local_register(2 as RegisterSize)
        .with_string(|s| s.to_string())?;
    let payload = context
        .resolve_local_register(3 as RegisterSize)
        .with_string(|s| s.to_string())?;
    efun::send_to_connection(context, ConnectionOp::Gmcp { package, payload });
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::vm::Vm,
        telnet::ops::ConnectionOp,
        test_support::{connect, test_config},
    };

    #[tokio::test]
    async fn a_message_is_queued_on_the_targets_connection() {
        let vm = Vm::new(test_config());
        let player = vm.create_process_from_code("/player.c", "").await.unwrap();
        let mut connected = connect(&vm, &player).await;
        let main = indoc! { r#"
            void create() { send_gmcp(find_object("/player"), "Char.Vitals", "{\"hp\":1}"); }
        "# };
        vm.initialize_process_from_code("/main.c", main)
            .await
            .unwrap();
        assert_eq!(
            connected.rx.try_recv(),
            Ok(ConnectionOp::Gmcp {
                package: "Char.Vitals".into(),
                payload: "{\"hp\":1}".into(),
            })
        );
    }

    #[tokio::test]
    async fn a_target_without_a_connection_is_silent() {
        let vm = Vm::new(test_config());
        vm.create_process_from_code("/npc.c", "").await.unwrap();
        let main = indoc! { r#"
            int done;
            void create() { send_gmcp(find_object("/npc"), "Core.Ping", ""); done = 1; }
        "# };
        let main = vm
            .initialize_process_from_code("/main.c", main)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            crate::interpreter::CommittedReader::committed_global(&vm.global_state, &main, 0u16),
            crate::interpreter::lpc_ref::LpcRef::from(1)
        );
    }
}
