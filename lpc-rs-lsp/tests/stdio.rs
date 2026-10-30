use std::{
    fs::{self, File},
    io::{self, BufReader, Write},
    process::{Child, Command, Stdio},
    thread,
    time::{Duration, Instant},
};

use flume::Receiver;
use lsp_server::{Message, Notification, Request, Response};
use serde_json::{Value, json};

struct ServerProcess {
    child: Child,
    reader: Option<thread::JoinHandle<()>>,
}

impl Drop for ServerProcess {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
        if let Some(reader) = self.reader.take() {
            let _ = reader.join();
        }
    }
}

fn send(input: &mut impl Write, message: Message) {
    message.write(input).unwrap();
    input.flush().unwrap();
}

fn response(messages: &Receiver<io::Result<Message>>, id: i32) -> Response {
    let deadline = Instant::now() + Duration::from_secs(15);
    loop {
        assert!(Instant::now() < deadline, "no response to request {id}");
        let message = messages
            .recv_timeout(deadline.saturating_duration_since(Instant::now()))
            .expect("language server stopped responding")
            .expect("invalid language-server message");
        if let Message::Response(response) = message
            && response.id == id.into()
        {
            return response;
        }
    }
}

#[test]
fn stdio_initialization_and_shutdown_reach_the_server() {
    let directory = tempfile::tempdir().unwrap();
    let log = directory.path().join("server.log");
    let child = Command::new(env!("CARGO_BIN_EXE_lpc-rs-lsp"))
        .arg("--lib-dir")
        .arg(directory.path())
        .current_dir(directory.path())
        .env_clear()
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(File::create(&log).unwrap())
        .spawn()
        .unwrap();
    let mut server = ServerProcess {
        child,
        reader: None,
    };
    let mut input = server.child.stdin.take().unwrap();
    let output = server.child.stdout.take().unwrap();
    let (sender, messages) = flume::unbounded();
    server.reader = Some(thread::spawn(move || {
        let mut output = BufReader::new(output);
        loop {
            let message = match Message::read(&mut output) {
                Ok(Some(message)) => Ok(message),
                Ok(None) => break,
                Err(error) => Err(error),
            };
            let failed = message.is_err();
            if sender.send(message).is_err() || failed {
                break;
            }
        }
    }));
    send(
        &mut input,
        Request::new(1.into(), "initialize".into(), json!({ "capabilities": {} })).into(),
    );
    let initialized = response(&messages, 1).response_result.unwrap();
    assert_eq!(initialized["capabilities"]["positionEncoding"], "utf-16");
    assert_eq!(initialized["serverInfo"]["name"], "lpc-rs-lsp");
    send(
        &mut input,
        Notification::new("initialized".into(), json!({})).into(),
    );
    send(
        &mut input,
        Request::new(2.into(), "shutdown".into(), Value::Null).into(),
    );
    assert_eq!(response(&messages, 2).response_result.unwrap(), Value::Null);
    send(
        &mut input,
        Notification::new("exit".into(), Value::Null).into(),
    );
    drop(input);
    let deadline = Instant::now() + Duration::from_secs(15);
    let status = loop {
        if let Some(status) = server.child.try_wait().unwrap() {
            break status;
        }
        assert!(Instant::now() < deadline, "language server did not exit");
        thread::sleep(Duration::from_millis(20));
    };
    assert!(status.success(), "{}", fs::read_to_string(log).unwrap());
}
