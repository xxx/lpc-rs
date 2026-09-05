use lpc_rs_errors::Result;
use lpc_rs_utils::lpc_string::LpcString;

use crate::interpreter::efun::{self, efun_context::EfunContext};

/// `query_ip_number`, an efun returning the address of an object's
/// connection as text; 0 without one.
pub fn query_ip_number<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    if let Some(connection) = efun::connection_of(context) {
        let ip = LpcString::from(connection.address.ip().to_string());
        context.return_efun_result(ip.into());
    }
    Ok(())
}

/// `query_ip_name`, an efun returning the host name of an object's
/// connection: its address until reverse DNS answers, or when it never
/// does; 0 without one.
pub fn query_ip_name<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    if let Some(connection) = efun::connection_of(context) {
        let name = match connection.host_name() {
            Some(name) => name.to_owned(),
            None => connection.address.ip().to_string(),
        };
        context.return_efun_result(LpcString::from(name).into());
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use std::sync::Arc;

    use crate::{
        interpreter::{
            CommittedReader, lpc_ref::LpcRef, process::Process, task::Task,
            task::task_template::TaskTemplate, vm::Vm,
        },
        test_support::{Connected, connect, test_config},
    };

    #[tokio::test]
    async fn the_ip_of_this_player() {
        let vm = Vm::new(test_config());
        let code = indoc! { r#"
            string ip;
            void create() { set_this_player(this_object()); ip = query_ip_number(); }
        "# };
        let player = vm
            .create_process_from_code("/player.c", code)
            .await
            .unwrap();
        let _connected = connect(&vm, &player).await;
        Task::<16>::initialize_process(
            TaskTemplate::from(vm.global_state.clone()).into_task_context(player.clone()),
        )
        .await
        .unwrap();
        assert_eq!(
            vm.global_state.committed_global(&player, 0u16).to_string(),
            "127.0.0.1"
        );
    }

    #[tokio::test]
    async fn no_connection_is_zero() {
        let vm = Vm::new(test_config());
        let main = indoc! { r#"
            mixed ip;
            void create() { ip = query_ip_number(this_object()); }
        "# };
        let main = vm
            .initialize_process_from_code("/main.c", main)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            vm.global_state.committed_global(&main, 0u16),
            LpcRef::from(0)
        );
    }

    async fn ip_name_of_this_player(vm: &Vm) -> (Arc<Process>, Connected) {
        let code = indoc! { r#"
            mixed name;
            void create() { set_this_player(this_object()); name = query_ip_name(); }
        "# };
        let player = vm
            .create_process_from_code("/player.c", code)
            .await
            .unwrap();
        let connected = connect(vm, &player).await;
        (player, connected)
    }

    async fn initialize(vm: &Vm, player: &Arc<Process>) {
        Task::<16>::initialize_process(
            TaskTemplate::from(vm.global_state.clone()).into_task_context(player.clone()),
        )
        .await
        .unwrap();
    }

    #[tokio::test]
    async fn the_name_is_the_address_until_reverse_dns_answers() {
        let vm = Vm::new(test_config());
        let (player, _connected) = ip_name_of_this_player(&vm).await;
        initialize(&vm, &player).await;
        assert_eq!(
            vm.global_state.committed_global(&player, 0u16).to_string(),
            "127.0.0.1"
        );
    }

    #[tokio::test]
    async fn the_name_is_the_host_name_once_known() {
        let vm = Vm::new(test_config());
        let (player, connected) = ip_name_of_this_player(&vm).await;
        connected.connection.set_host_name("localhost".to_owned());
        initialize(&vm, &player).await;
        assert_eq!(
            vm.global_state.committed_global(&player, 0u16).to_string(),
            "localhost"
        );
    }

    #[tokio::test]
    async fn no_connection_has_no_name() {
        let vm = Vm::new(test_config());
        let main = indoc! { r#"
            mixed name;
            void create() { name = query_ip_name(this_object()); }
        "# };
        let main = vm
            .initialize_process_from_code("/main.c", main)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            vm.global_state.committed_global(&main, 0u16),
            LpcRef::from(0)
        );
    }
}
