//! An end-to-end smoke of the telnet stack: boots the real driver against
//! `tests/fixtures/mudlib` on real TCP ports and walks login, negotiation,
//! GMCP, MXP, MSSP, NAWS, commands, idle, and disconnect.
//!
//! Ignored by default — it binds fixed ports. Run it explicitly:
//! `cargo test --test telnet_smoke_test -- --ignored`

use std::time::Duration;

use lpc_rs::interpreter::vm::Vm;
use lpc_rs_utils::config::ConfigBuilder;
use tokio::{
    io::{AsyncReadExt, AsyncWriteExt},
    net::TcpStream,
    task::JoinHandle,
    time::{sleep, timeout},
};

const IAC: u8 = 255;
const SB: u8 = 250;
const SE: u8 = 240;
const WILL: u8 = 251;
const DO: u8 = 253;
const GA: u8 = 249;
const EOR_CMD: u8 = 239;
const NAWS: u8 = 31;
const EOR: u8 = 25;
const MSSP: u8 = 70;
const MXP: u8 = 91;
const GMCP: u8 = 201;

const SMOKE_PORT: u16 = 24951;
const IDLE_PORT: u16 = 24952;

async fn boot(port: u16, max_idle_time: u64) -> JoinHandle<()> {
    let config = ConfigBuilder::default()
        .lib_dir("./tests/fixtures/mudlib")
        .port(port)
        .max_idle_time(max_idle_time)
        .build()
        .unwrap();
    let mut vm = Vm::new(config);
    tokio::spawn(async move {
        vm.boot().await.expect("the driver boots");
    })
}

async fn connect(port: u16) -> TcpStream {
    for _ in 0..100 {
        if let Ok(stream) = TcpStream::connect(("127.0.0.1", port)).await {
            return stream;
        }
        sleep(Duration::from_millis(100)).await;
    }
    panic!("the driver never listened on {port}");
}

/// Read until `needle` appears in the accumulated bytes, or panic after two
/// seconds with everything read so far.
async fn read_until(stream: &mut TcpStream, needle: &[u8]) -> Vec<u8> {
    let mut buf = Vec::new();
    let deadline = Duration::from_secs(2);
    let mut chunk = [0u8; 4096];
    loop {
        if buf.windows(needle.len().max(1)).any(|w| w == needle) {
            return buf;
        }
        match timeout(deadline, stream.read(&mut chunk)).await {
            Ok(Ok(0)) => panic!("EOF while waiting for {needle:?}; got {buf:?}"),
            Ok(Ok(n)) => buf.extend_from_slice(&chunk[..n]),
            Ok(Err(e)) => panic!("read failed: {e}"),
            Err(_) => panic!("timed out waiting for {needle:?}; got {buf:?}"),
        }
    }
}

/// Read until the peer closes the stream; everything received on the way.
async fn read_to_eof(stream: &mut TcpStream) -> Vec<u8> {
    let mut buf = Vec::new();
    let mut chunk = [0u8; 4096];
    loop {
        match timeout(Duration::from_secs(5), stream.read(&mut chunk)).await {
            Ok(Ok(0)) => return buf,
            Ok(Ok(n)) => buf.extend_from_slice(&chunk[..n]),
            Ok(Err(_)) => return buf,
            Err(_) => panic!("timed out waiting for EOF; got {buf:?}"),
        }
    }
}

fn contains(haystack: &[u8], needle: &[u8]) -> bool {
    haystack.windows(needle.len().max(1)).any(|w| w == needle)
}

#[tokio::test]
#[ignore = "boots the real driver on fixed TCP ports"]
async fn the_whole_telnet_surface_over_a_real_socket() {
    let driver = boot(SMOKE_PORT, 0).await;
    let mut client = connect(SMOKE_PORT).await;

    // The greeting offers the extensions and marks the login prompt with GA.
    let greeting = read_until(&mut client, &[IAC, GA]).await;
    for offer in [EOR, MSSP, MXP, GMCP] {
        assert!(contains(&greeting, &[IAC, WILL, offer]), "offers {offer}");
    }
    assert!(contains(&greeting, &[IAC, DO, NAWS]), "asks for NAWS");
    assert!(contains(&greeting, b"What is your name? "), "{greeting:?}");

    // Accept everything offered and report a 100x40 window.
    client
        .write_all(&[
            IAC, DO, GMCP, IAC, DO, EOR, IAC, DO, MSSP, IAC, DO, MXP, IAC, WILL, NAWS, IAC, SB,
            NAWS, 0, 100, 0, 40, IAC, SE,
        ])
        .await
        .unwrap();
    let answers = read_until(&mut client, b"[naws] 100x40\r\n").await;
    assert!(
        contains(&answers, b"\x01NAME\x02lpc-rs smoke"),
        "MSSP carries the master's NAME: {answers:?}"
    );
    assert!(
        contains(&answers, b"\x01FAMILY\x02LPMud"),
        "and the master's extra variable: {answers:?}"
    );

    // Logging in lands in the room; the prompt gets the negotiated EOR mark.
    client.write_all(b"smoke\r\n").await.unwrap();
    let login = read_until(&mut client, &[IAC, EOR_CMD]).await;
    assert!(contains(&login, b"Welcome, smoke!"), "{login:?}");
    assert!(
        contains(&login, b"The place where it all begins."),
        "{login:?}"
    );
    assert!(contains(&login, b"smoke> "), "{login:?}");

    // The connection answers query_connection through the stats verb.
    client.write_all(b"stats\r\n").await.unwrap();
    let stats = read_until(&mut client, &[IAC, EOR_CMD]).await;
    for line in [
        b"cols: 100\r\n".as_slice(),
        b"rows: 40\r\n",
        b"gmcp: 1\r\n",
        b"mxp: 1\r\n",
        b"eor: 1\r\n",
    ] {
        assert!(contains(&stats, line), "{stats:?}");
    }

    // A GMCP message from the client reaches the body's gmcp() apply.
    let mut hello = vec![IAC, SB, GMCP];
    hello.extend_from_slice(b"Core.Hello { \"client\": \"smoke test\" }");
    hello.extend([IAC, SE]);
    client.write_all(&hello).await.unwrap();
    read_until(
        &mut client,
        b"[gmcp] Core.Hello { \"client\": \"smoke test\" }\r\n",
    )
    .await;

    // send_gmcp rides out as a GMCP subnegotiation.
    client.write_all(b"gmcp\r\n").await.unwrap();
    let out = read_until(&mut client, &[IAC, EOR_CMD]).await;
    let mut echo = vec![IAC, SB, GMCP];
    echo.extend_from_slice(b"Smoke.Echo { \"who\": \"smoke\" }");
    echo.extend([IAC, SE]);
    assert!(contains(&out, &echo), "{out:?}");

    // send_mxp markup rides out as a secure line.
    client.write_all(b"mxp\r\n").await.unwrap();
    let mxp = read_until(&mut client, &[IAC, EOR_CMD]).await;
    assert!(contains(&mxp, b"\x1b[1z<b>bold</b>\r\n"), "{mxp:?}");

    // A window resize reaches the window_size() apply.
    client
        .write_all(&[IAC, SB, NAWS, 0, 80, 0, 24, IAC, SE])
        .await
        .unwrap();
    read_until(&mut client, b"[naws] 80x24\r\n").await;

    // A verb nothing registered gets the driver's fallback.
    client.write_all(b"frobnicate\r\n").await.unwrap();
    let unknown = read_until(&mut client, &[IAC, EOR_CMD]).await;
    assert!(contains(&unknown, b"What?\r\n"), "{unknown:?}");

    // quit destructs the body, which closes the connection.
    client.write_all(b"quit\r\n").await.unwrap();
    let goodbye = read_to_eof(&mut client).await;
    assert!(contains(&goodbye, b"Goodbye, smoke!"), "{goodbye:?}");

    driver.abort();
}

#[tokio::test]
#[ignore = "boots the real driver on fixed TCP ports"]
async fn an_idle_connection_is_kicked_with_a_goodbye() {
    let driver = boot(IDLE_PORT, 1).await;
    let mut client = connect(IDLE_PORT).await;
    read_until(&mut client, b"What is your name? ").await;

    // No input for over max_idle_time: the goodbye, then the close.
    let kicked = read_to_eof(&mut client).await;
    assert!(
        contains(&kicked, b"*** Disconnected: idle too long ***"),
        "{kicked:?}"
    );

    driver.abort();
}
