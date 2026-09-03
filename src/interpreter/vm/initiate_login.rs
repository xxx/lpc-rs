use std::sync::Arc;

use lpc_rs_core::LpcIntInner;
use lpc_rs_errors::{LpcError, lpc_error};
use lpc_rs_utils::lpc_string::LpcString;
use tracing::{debug, instrument};

use crate::{
    interpreter::{
        CONNECT, LOGON,
        lpc_ref::LpcRef,
        process::Process,
        task::{
            apply_function::{apply_function_by_name, report_runtime_error},
            task_template::TaskTemplate,
        },
        vm::Vm,
    },
    telnet::{connection::Connection, ops::ConnectionOp},
};

impl Vm {
    /// Start the login process for a [`Connection`]. This assumes the connection is not
    /// already logged in and attached to an object.
    #[instrument(skip_all)]
    pub async fn initiate_login(&self, connection: Arc<Connection>) {
        let global_state = self.global_state.clone();
        let task_template = TaskTemplate::from(self.global_state.clone());

        let address = connection.address;
        let (ip, port) = (address.ip().to_string(), address.port());

        // call 'connect' in the master object
        let ip_ref = LpcRef::from(LpcString::from(ip));
        let port_ref = LpcRef::from(port as LpcIntInner);

        tokio::spawn(async move {
            debug!("initiating login for {}", connection.address);

            // Abort the login; no object to blame means the master object
            // itself is bad, so `error_handler` is not applied.
            let fail = async |error: LpcError, object: Option<Arc<Process>>| {
                global_state
                    .detach(&connection, Some(error.to_string()))
                    .await;

                if object.is_some() {
                    let template = TaskTemplate::from(global_state.clone());
                    report_runtime_error(&error, object, template).await;
                }
            };

            // get the master object
            let Some(master) = global_state.object_space.master_object() else {
                fail(
                    lpc_error!("Fatal server error - Failed to get master object."),
                    None,
                )
                .await;
                return;
            };

            let maybe_login_ob = match apply_function_by_name(
                CONNECT,
                &[ip_ref.clone(), port_ref.clone()],
                master.clone(),
                task_template.clone(),
                Some(task_template.global_state.config.max_execution_time),
            )
            .await
            {
                Some(Ok(LpcRef::Object(ob))) => ob,
                Some(Ok(r)) => {
                    if let LpcRef::String(_) = r {
                        let message = r
                            .with_string(|s| s.to_string())
                            .unwrap_or_else(|_| "No message received?".to_string());
                        global_state.detach(&connection, Some(message)).await;
                        return;
                    }

                    fail(lpc_error!("Fatal server error - We didn't receive an object back when calling connect(). Received {}", r.type_name()), Some(master)).await;
                    return;
                }
                Some(Err(e)) => {
                    fail(e, Some(master)).await;
                    return;
                }
                None => {
                    fail(lpc_error!("Fatal server error - Unable to find the `connect` function in the master object."), Some(master)).await;
                    return;
                }
            };

            let Some(login_ob) = maybe_login_ob.upgrade() else {
                debug_assert!(
                    false,
                    "We received a destructed object back when calling connect(). This should never happen."
                );
                fail(lpc_error!("Fatal server error - We received a destructed object back when calling connect()."), Some(master)).await;
                return;
            };

            // This is the initial exec() of the player into a body.
            global_state
                .attach(connection.clone(), login_ob.clone())
                .await;

            let template = task_template.clone();
            template.set_this_player(Some(login_ob.clone()));

            // call 'logon' in the login object
            let max_execution_time = task_template.global_state.config.max_execution_time;
            match apply_function_by_name(
                LOGON,
                &[ip_ref, port_ref],
                login_ob.clone(),
                template,
                Some(max_execution_time),
            )
            .await
            {
                Some(Ok(LpcRef::Int(i))) => {
                    if i == 0 {
                        // logon() sent its own messages; nothing is added.
                        global_state.detach(&connection, None).await;
                        return;
                    }
                }
                Some(Ok(_)) => {
                    fail(lpc_error!("Fatal server error - We didn't receive an int back when calling logon()."), Some(login_ob)).await;
                    return;
                }
                Some(Err(e)) => {
                    fail(e, Some(login_ob)).await;
                    return;
                }
                None => {
                    fail(lpc_error!("Fatal server error - Unable to find the `logon` function in the object."), Some(login_ob)).await;
                    return;
                }
            }

            // No command line ran, so nothing else asks for the cycle that
            // marks logon()'s first prompt.
            let _ = connection.send(ConnectionOp::PromptCycle);
            // A client that left mid-login is not a player.
            if !connection.is_dead() {
                connection.set_logged_in();
            }
        });
    }
}

#[cfg(test)]
mod tests {
    use std::{net::ToSocketAddrs, time::Duration};

    use indoc::indoc;
    use tokio::sync::mpsc;

    use super::*;
    use crate::{interpreter::CommittedReader, test_support::test_config};

    /// A master that is also the login object its `connect()` hands back.
    const MASTER: &str = indoc! { r#"
        object connect(string ip, int port) { return this_object(); }

        int logon(string ip, int port) {
            write("Name: ");
            input_to(get_name);
            return 1;
        }

        void get_name(string s) {}
    "# };

    async fn within<F: std::future::Future>(f: F) -> F::Output {
        tokio::time::timeout(Duration::from_secs(5), f)
            .await
            .expect("the login finishes within five seconds")
    }

    /// Poll `probe` until it holds, within the timeout.
    async fn eventually<F: Fn() -> bool>(probe: F) {
        within(async {
            while !probe() {
                tokio::time::sleep(Duration::from_millis(5)).await;
            }
        })
        .await
    }

    /// A [`Vm`] with `master` as `/secure/master.c`, and its process.
    async fn vm_with_master(master: &str) -> (Vm, Arc<Process>) {
        let vm = Vm::new(test_config());
        let process = vm
            .global_state
            .initialize_process_from_code("/secure/master.c", master)
            .await
            .expect("the master compiles")
            .context
            .process;
        assert!(
            vm.global_state.object_space.master_object().is_some(),
            "the master is registered"
        );
        (vm, process)
    }

    fn fresh_connection() -> (Arc<Connection>, mpsc::UnboundedReceiver<ConnectionOp>) {
        let (tx, rx) = mpsc::unbounded_channel();
        let address = "127.0.0.1:4000"
            .to_socket_addrs()
            .expect("a literal address")
            .next()
            .expect("one address");
        (Arc::new(Connection::new(address, tx)), rx)
    }

    #[tokio::test]
    async fn the_first_prompt_gets_its_cycle() {
        let (vm, _master) = vm_with_master(MASTER).await;
        let (connection, mut rx) = fresh_connection();
        vm.initiate_login(connection.clone()).await;

        let mut ops = Vec::new();
        for _ in 0..4 {
            ops.push(within(rx.recv()).await.expect("the login task is running"));
        }
        assert_eq!(ops[0], ConnectionOp::Attached, "attach binds the body");
        // `input_to` reaches the connection as it runs; `write` is an effect,
        // so its message follows at the commit.
        assert!(matches!(ops[1], ConnectionOp::InputTo(_)));
        assert_eq!(ops[2], ConnectionOp::SendMessage("Name: ".into()));
        assert_eq!(
            ops[3],
            ConnectionOp::PromptCycle,
            "the first prompt asks for its mark"
        );
        eventually(|| connection.is_logged_in()).await;
    }

    #[tokio::test]
    async fn a_refusing_connect_sends_its_reason_then_closes() {
        let master = indoc! { r#"
            mixed connect(string ip, int port) { return "Go away.\n"; }
            int logon(string ip, int port) { return 1; }
        "# };
        let (vm, _master) = vm_with_master(master).await;
        let (connection, mut rx) = fresh_connection();
        vm.initiate_login(connection.clone()).await;

        assert_eq!(
            within(rx.recv()).await,
            Some(ConnectionOp::SendMessage("Go away.\n".into()))
        );
        assert_eq!(within(rx.recv()).await, Some(ConnectionOp::Close));
        assert!(connection.is_dead());
        assert!(!connection.is_logged_in());
    }

    #[tokio::test]
    async fn a_connect_that_errors_is_told_why_and_handled() {
        let master = indoc! { r#"
            int handled;
            mixed connect(string ip, int port) { throw("no entry"); }
            int logon(string ip, int port) { return 1; }
            void error_handler(mapping e) { handled = 1; }
        "# };
        let (vm, master) = vm_with_master(master).await;
        let (connection, mut rx) = fresh_connection();
        vm.initiate_login(connection.clone()).await;

        let first = within(rx.recv()).await.expect("the login task is running");
        assert!(
            matches!(&first, ConnectionOp::SendMessage(text) if text.contains("no entry")),
            "{first:?}"
        );
        assert_eq!(within(rx.recv()).await, Some(ConnectionOp::Close));
        assert!(connection.is_dead());
        assert!(!connection.is_logged_in());

        eventually(|| vm.global_state.committed_global(&master, 0u16) == LpcRef::from(1)).await;
    }

    #[tokio::test]
    async fn a_logon_returning_zero_unbinds_the_login_object() {
        let master = indoc! { r#"
            object connect(string ip, int port) { return this_object(); }
            int logon(string ip, int port) { return 0; }
        "# };
        let (vm, master) = vm_with_master(master).await;
        let (connection, mut rx) = fresh_connection();
        vm.initiate_login(connection.clone()).await;

        assert_eq!(within(rx.recv()).await, Some(ConnectionOp::Attached));
        assert_eq!(within(rx.recv()).await, Some(ConnectionOp::Close));
        assert!(
            vm.global_state.committed_connection(&master).is_none(),
            "the cell a failed logon used to leak"
        );
        assert!(connection.body().is_none());
        assert!(!connection.is_logged_in());
    }
}
