use std::time::Duration;

use lpc_rs_errors::{Result, lpc_error};
use lpc_rs_utils::tls::{CertificateFiles, Identity, outside_mudlib};
use tokio::{
    net::{TcpListener, ToSocketAddrs},
    task::{JoinHandle, JoinSet},
};
use tokio_rustls::TlsAcceptor;
use tracing::{info, warn};

use super::Telnet;
use crate::interpreter::task::task_template::TaskTemplate;

const RELOAD_INTERVAL: Duration = Duration::from_secs(60);
const HANDSHAKE_TIMEOUT: Duration = Duration::from_secs(10);
const MAX_HANDSHAKES: usize = 128;

#[derive(Debug)]
pub(super) struct Acceptors(Vec<JoinHandle<()>>);

impl Drop for Acceptors {
    fn drop(&mut self) {
        for task in &self.0 {
            task.abort();
        }
    }
}

pub(super) async fn bind<A: ToSocketAddrs>(
    address: A,
    template: TaskTemplate,
) -> Result<Acceptors> {
    let config = &template.global_state.config;
    let tls = if let Some(tls) = &config.tls {
        let identity = load(tls.files.clone(), config.lib_dir.to_string()).await?;
        let listener = TcpListener::bind((config.bind_address.as_str(), tls.port))
            .await
            .map_err(|e| lpc_error!("TLS failed to bind its listener: {e}"))?;
        Some((listener, tls.files.clone(), identity))
    } else {
        None
    };
    let plain = if config.telnet_enabled {
        Some(
            TcpListener::bind(address)
                .await
                .map_err(|e| lpc_error!("telnet failed to bind its listener: {e}"))?,
        )
    } else {
        None
    };
    let mut tasks = Acceptors(Vec::new());
    if let Some(listener) = plain {
        info!(
            "Listening for plaintext connections on {}",
            listener.local_addr().map_err(|e| lpc_error!("{e}"))?
        );
        tasks
            .0
            .push(tokio::spawn(accept_plain(listener, template.clone())));
    }
    if let Some((listener, files, identity)) = tls {
        info!(
            "Listening for TLS connections on {}",
            listener.local_addr().map_err(|e| lpc_error!("{e}"))?
        );
        tasks.0.push(tokio::spawn(accept_tls(
            listener,
            template,
            files,
            identity,
            RELOAD_INTERVAL,
        )));
    }
    Ok(tasks)
}

async fn accept_plain(listener: TcpListener, template: TaskTemplate) {
    loop {
        match listener.accept().await {
            Ok((stream, address)) => {
                tokio::spawn(Telnet::connection_loop(stream, address, template.clone()));
            }
            Err(e) => accept_error(e).await,
        }
    }
}

async fn load(files: CertificateFiles, lib_dir: String) -> Result<Identity> {
    tokio::task::spawn_blocking(move || {
        outside_mudlib(&files.private_key, &lib_dir)?;
        files.load()
    })
    .await
    .map_err(|e| lpc_error!("TLS certificate loader failed: {e}"))?
}

async fn accept_tls(
    listener: TcpListener,
    template: TaskTemplate,
    files: CertificateFiles,
    mut identity: Identity,
    reload_interval: Duration,
) {
    let mut reload = tokio::time::interval(reload_interval);
    reload.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);
    reload.tick().await;
    let mut handshakes = JoinSet::new();
    loop {
        tokio::select! {
            _ = reload.tick() => {
                match load(files.clone(), template.global_state.config.lib_dir.to_string()).await {
                    Ok(replacement) if !identity.same_certificate(&replacement) => {
                        identity = replacement;
                        info!("Reloaded TLS certificate");
                    }
                    Ok(_) => {}
                    Err(e) => warn!("TLS certificate reload failed; retaining previous identity: {e}"),
                }
            }
            result = listener.accept(), if handshakes.len() < MAX_HANDSHAKES => {
                match result {
                    Ok((stream, address)) => {
                        if let Err(e) = identity.check_validity() {
                            warn!("Rejecting TLS connection from {address}: {e}");
                            continue;
                        }
                        let acceptor = TlsAcceptor::from(identity.server_config.clone());
                        handshakes.spawn(async move {
                            match tokio::time::timeout(HANDSHAKE_TIMEOUT, acceptor.accept(stream)).await {
                                Ok(Ok(stream)) => Some((stream, address)),
                                Ok(Err(e)) => { warn!("TLS handshake from {address} failed: {e}"); None }
                                Err(_) => { warn!("TLS handshake from {address} timed out"); None }
                            }
                        });
                    }
                    Err(e) => accept_error(e).await,
                }
            }
            result = handshakes.join_next(), if !handshakes.is_empty() => {
                if let Some(Ok(Some((stream, address)))) = result {
                    tokio::spawn(Telnet::connection_loop(stream, address, template.clone()));
                }
            }
        }
    }
}

async fn accept_error(error: std::io::Error) {
    warn!("accept failed: {error}");
    // A persistent descriptor limit must not spin the accept loop.
    tokio::time::sleep(Duration::from_millis(100)).await;
}

#[cfg(test)]
mod tests {
    use std::{fs, net::SocketAddr, path::Path, sync::Arc};

    use lpc_rs_utils::{config::ConfigBuilder, tls::TlsConfig};
    use rcgen::{CertifiedKey, KeyPair, generate_simple_self_signed};
    use tokio::{
        io::{AsyncRead, AsyncReadExt, AsyncWriteExt},
        net::TcpStream,
    };
    use tokio_rustls::{
        TlsConnector,
        client::TlsStream,
        rustls::{ClientConfig, RootCertStore, pki_types::ServerName},
    };

    use super::*;
    use crate::{interpreter::vm::Vm, test_support::test_config};

    fn bundle(files: &CertificateFiles, generated: &CertifiedKey<KeyPair>) {
        let temporary = files.certificate.with_extension("tmp");
        fs::write(
            &temporary,
            format!(
                "{}{}",
                generated.cert.pem(),
                generated.signing_key.serialize_pem()
            ),
        )
        .unwrap();
        fs::rename(temporary, &files.certificate).unwrap();
    }

    fn files(dir: &Path) -> CertificateFiles {
        let path = dir.join("identity.pem");
        CertificateFiles {
            certificate: path.clone(),
            private_key: path,
        }
    }

    fn connector(certificates: &[&CertifiedKey<KeyPair>]) -> TlsConnector {
        let mut roots = RootCertStore::empty();
        for generated in certificates {
            roots.add(generated.cert.der().clone()).unwrap();
        }
        let config = ClientConfig::builder_with_provider(Arc::new(
            tokio_rustls::rustls::crypto::ring::default_provider(),
        ))
        .with_safe_default_protocol_versions()
        .unwrap()
        .with_root_certificates(roots)
        .with_no_client_auth();
        TlsConnector::from(Arc::new(config))
    }

    async fn connect(connector: &TlsConnector, address: SocketAddr) -> TlsStream<TcpStream> {
        tokio::time::timeout(Duration::from_secs(3), async {
            connector
                .connect(
                    ServerName::try_from("localhost").unwrap(),
                    TcpStream::connect(address).await.unwrap(),
                )
                .await
                .unwrap()
        })
        .await
        .unwrap()
    }

    async fn read_until<S: AsyncRead + Unpin>(stream: &mut S, needle: &[u8]) -> Vec<u8> {
        tokio::time::timeout(Duration::from_secs(3), async {
            let mut bytes = Vec::new();
            let mut buffer = [0; 4096];
            while !bytes.windows(needle.len()).any(|s| s == needle) {
                let n = stream.read(&mut buffer).await.unwrap();
                assert_ne!(n, 0, "EOF waiting for {needle:?}: {bytes:?}");
                bytes.extend_from_slice(&buffer[..n]);
            }
            bytes
        })
        .await
        .unwrap()
    }

    #[tokio::test]
    async fn tls_login_commands_and_reload_preserve_existing_players() {
        let dir = tempfile::tempdir().unwrap();
        let files = files(dir.path());
        let original = generate_simple_self_signed(vec!["localhost".into()]).unwrap();
        let replacement = generate_simple_self_signed(vec!["localhost".into()]).unwrap();
        bundle(&files, &original);
        let connector = connector(&[&original, &replacement]);
        let config = ConfigBuilder::default()
            .lib_dir("tests/fixtures/mudlib")
            .build()
            .unwrap();
        let mut vm = Vm::new(config);
        vm.bootstrap().await.unwrap();
        let template = TaskTemplate::from(vm.global_state.clone());
        let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let address = listener.local_addr().unwrap();
        let tls_task = tokio::spawn(accept_tls(
            listener,
            template,
            files.clone(),
            files.load().unwrap(),
            Duration::from_millis(20),
        ));
        let _tasks = Acceptors(vec![
            tls_task,
            tokio::spawn(async move {
                vm.run().await.unwrap();
            }),
        ]);

        let mut player = connect(&connector, address).await;
        let greeting = read_until(&mut player, b"What is your name? ").await;
        assert!(
            greeting.windows(3).any(|s| s == [255, 251, 201]),
            "GMCP is offered inside TLS"
        );
        player.write_all(b"secure\r\n").await.unwrap();
        player.flush().await.unwrap();
        let login = read_until(&mut player, b"secure> ").await;
        assert!(
            login
                .windows(b"Welcome, secure!".len())
                .any(|part| part == b"Welcome, secure!")
        );

        fs::write(&files.certificate, "broken replacement").unwrap();
        tokio::time::sleep(Duration::from_millis(70)).await;
        let survivor = connect(&connector, address).await;
        assert_eq!(
            survivor.get_ref().1.peer_certificates().unwrap()[0],
            *original.cert.der()
        );
        drop(survivor);

        bundle(&files, &replacement);
        tokio::time::timeout(Duration::from_secs(3), async {
            loop {
                let fresh = connect(&connector, address).await;
                if fresh.get_ref().1.peer_certificates().unwrap()[0] == *replacement.cert.der() {
                    break;
                }
                tokio::time::sleep(Duration::from_millis(20)).await;
            }
        })
        .await
        .unwrap();
        player.write_all(b"look\r\n").await.unwrap();
        player.flush().await.unwrap();
        read_until(&mut player, b"The place where it all begins.").await;
        player.write_all(b"quit\r\n").await.unwrap();
        player.flush().await.unwrap();
        let mut remainder = Vec::new();
        tokio::time::timeout(Duration::from_secs(3), player.read_to_end(&mut remainder))
            .await
            .unwrap()
            .unwrap();
    }

    #[tokio::test]
    async fn listener_shutdown_cancels_handshakes_and_plaintext_is_rejected() {
        let dir = tempfile::tempdir().unwrap();
        let files = files(dir.path());
        let cert = generate_simple_self_signed(vec!["localhost".into()]).unwrap();
        bundle(&files, &cert);
        let vm = Vm::new(test_config());
        let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let address = listener.local_addr().unwrap();
        let task = tokio::spawn(accept_tls(
            listener,
            TaskTemplate::from(vm.global_state.clone()),
            files.clone(),
            files.load().unwrap(),
            RELOAD_INTERVAL,
        ));
        let tasks = Acceptors(vec![task]);
        let mut plaintext = TcpStream::connect(address).await.unwrap();
        plaintext
            .write_all(b"GET / HTTP/1.0\r\n\r\n")
            .await
            .unwrap();
        let mut response = Vec::new();
        let _ = tokio::time::timeout(Duration::from_secs(3), plaintext.read_to_end(&mut response))
            .await
            .unwrap();
        assert!(
            response.is_empty() || response[0] == 21,
            "only a TLS alert may precede rejection"
        );
        assert!(vm.global_state.registry.is_empty());
        let mut stalled = TcpStream::connect(address).await.unwrap();
        // A stalled peer must not block a successful handshake on another socket.
        let _working = connect(&connector(&[&cert]), address).await;
        drop(tasks);
        let mut buf = [0; 100];
        let result = tokio::time::timeout(Duration::from_secs(3), stalled.read(&mut buf))
            .await
            .unwrap();
        assert!(matches!(result, Ok(0) | Err(_)));
        assert!(TcpStream::connect(address).await.is_err());
    }

    #[tokio::test]
    async fn an_invalid_tls_identity_prevents_any_listener_from_starting() {
        let dir = tempfile::tempdir().unwrap();
        let config = ConfigBuilder::default()
            .tls(TlsConfig {
                port: 0,
                files: files(dir.path()),
            })
            .build()
            .unwrap();
        let vm = Vm::new(config);
        let telnet = Telnet::new();
        assert!(
            telnet
                .run("127.0.0.1:0", TaskTemplate::from(vm.global_state.clone()))
                .await
                .is_err()
        );
        assert!(telnet.handle.get().is_none());
    }

    #[tokio::test]
    async fn tls_only_configuration_never_binds_the_plaintext_address() {
        let dir = tempfile::tempdir().unwrap();
        let files = files(dir.path());
        bundle(
            &files,
            &generate_simple_self_signed(vec!["localhost".into()]).unwrap(),
        );
        let config = ConfigBuilder::default()
            .telnet_enabled(false)
            .tls(TlsConfig { port: 0, files })
            .build()
            .unwrap();
        let vm = Vm::new(config);
        let mut telnet = Telnet::new();
        telnet
            .run(
                "invalid plaintext address",
                TaskTemplate::from(vm.global_state.clone()),
            )
            .await
            .unwrap();
        assert_eq!(telnet.handle.get().unwrap().0.len(), 1);
        telnet.shutdown();
        assert!(telnet.handle.get().is_none());
    }

    #[tokio::test]
    async fn buffered_output_is_flushed_even_when_the_peer_sends_nothing() {
        let vm = Vm::new(test_config());
        let (mut client, server) = tokio::io::duplex(4096);
        let task = tokio::spawn(Telnet::connection_loop(
            tokio::io::BufStream::new(server),
            "127.0.0.1:4000".parse().unwrap(),
            TaskTemplate::from(vm.global_state.clone()),
        ));
        let _tasks = Acceptors(vec![task]);
        let mut greeting = [0; 21];
        tokio::time::timeout(Duration::from_secs(2), client.read_exact(&mut greeting))
            .await
            .unwrap()
            .unwrap();
        assert_eq!(&greeting[..3], &[255, 253, 31]);
    }
}
