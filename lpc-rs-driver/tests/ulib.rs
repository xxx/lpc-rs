//! Exercise a standalone copy with the installed-command interface and real clients.
#![cfg(unix)]

use std::{
    fs::{self, File},
    io::{Read, Write},
    net::{SocketAddr, TcpStream},
    path::{Path, PathBuf},
    process::{Child, Command, Stdio},
    thread::sleep,
    time::{Duration, Instant},
};

use tempfile::TempDir;

const TIMEOUT: Duration = Duration::from_secs(15);

fn copy_tree(source: &Path, destination: &Path) {
    fs::create_dir_all(destination).unwrap();
    for entry in fs::read_dir(source).unwrap() {
        let entry = entry.unwrap();
        let target = destination.join(entry.file_name());
        if entry.file_type().unwrap().is_dir() {
            copy_tree(&entry.path(), &target);
        } else {
            fs::copy(entry.path(), target).unwrap();
        }
    }
}

struct Driver {
    child: Child,
    log: PathBuf,
    _directory: TempDir,
}

impl Driver {
    fn start() -> (Self, SocketAddr) {
        let directory = tempfile::tempdir().unwrap();
        let mudlib = directory.path().join("my mud");
        copy_tree(
            &Path::new(env!("CARGO_MANIFEST_DIR")).join("../ulib"),
            &mudlib,
        );
        let log = directory.path().join("driver.log");
        let output = File::create(&log).unwrap();
        let child = Command::new(env!("CARGO_BIN_EXE_lpc-rs-driver"))
            .args(["--env", "driver.env"])
            .current_dir(mudlib)
            .env_clear()
            .env("LPC_PORT", "0")
            .stdin(Stdio::null())
            .stdout(output.try_clone().unwrap())
            .stderr(output)
            .spawn()
            .unwrap();
        let mut driver = Self {
            child,
            log,
            _directory: directory,
        };
        let deadline = Instant::now() + TIMEOUT;
        loop {
            let log = fs::read_to_string(&driver.log).unwrap();
            if let Some((_, tail)) = log.split_once("Listening for plaintext connections on ") {
                let address = tail.lines().next().unwrap().trim().parse().unwrap();
                return (driver, address);
            }
            assert!(
                driver.child.try_wait().unwrap().is_none(),
                "driver exited: {log}"
            );
            assert!(Instant::now() < deadline, "driver failed to listen: {log}");
            sleep(Duration::from_millis(20));
        }
    }

    fn shutdown(&mut self) {
        assert!(
            Command::new("kill")
                .args(["-INT", &self.child.id().to_string()])
                .status()
                .unwrap()
                .success()
        );
        let deadline = Instant::now() + TIMEOUT;
        loop {
            if let Some(status) = self.child.try_wait().unwrap() {
                assert!(
                    status.success(),
                    "driver failed: {}",
                    fs::read_to_string(&self.log).unwrap()
                );
                return;
            }
            assert!(Instant::now() < deadline, "driver did not shut down");
            sleep(Duration::from_millis(20));
        }
    }
}

impl Drop for Driver {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

struct Client {
    stream: TcpStream,
    pending: Vec<u8>,
}

impl Client {
    fn connect(address: SocketAddr) -> Self {
        let stream = TcpStream::connect_timeout(&address, TIMEOUT).unwrap();
        stream.set_write_timeout(Some(TIMEOUT)).unwrap();
        let mut client = Self {
            stream,
            pending: Vec::new(),
        };
        client.until("Name: ");
        client
    }

    fn send(&mut self, line: &str) {
        write!(self.stream, "{line}\r\n").unwrap();
    }

    fn until(&mut self, needle: &str) -> String {
        let deadline = Instant::now() + TIMEOUT;
        loop {
            if let Some(start) = self
                .pending
                .windows(needle.len())
                .position(|part| part == needle.as_bytes())
            {
                let matched: Vec<_> = self.pending.drain(..start + needle.len()).collect();
                return String::from_utf8_lossy(&matched).into_owned();
            }
            let remaining = deadline.saturating_duration_since(Instant::now());
            assert!(
                !remaining.is_zero(),
                "waiting for {needle:?}: {:?}",
                String::from_utf8_lossy(&self.pending)
            );
            self.stream.set_read_timeout(Some(remaining)).unwrap();
            let mut bytes = [0; 4096];
            let count = self.stream.read(&mut bytes).unwrap_or_else(|e| {
                panic!(
                    "waiting for {needle:?}: {e}; {:?}",
                    String::from_utf8_lossy(&self.pending)
                )
            });
            assert_ne!(
                count,
                0,
                "EOF waiting for {needle:?}: {:?}",
                String::from_utf8_lossy(&self.pending)
            );
            self.pending.extend_from_slice(&bytes[..count]);
        }
    }

    fn command(&mut self, name: &str, line: &str) -> String {
        self.send(line);
        self.until(&format!("{name}> "))
    }

    fn eof(&mut self) -> String {
        self.stream.set_read_timeout(Some(TIMEOUT)).unwrap();
        self.stream.read_to_end(&mut self.pending).unwrap();
        String::from_utf8_lossy(&std::mem::take(&mut self.pending)).into_owned()
    }
}

#[test]
fn copied_ulib_supports_guest_chat_and_connection_cleanup() {
    let (mut driver, address) = Driver::start();
    let mut alice = Client::connect(address);
    for invalid in [
        "",
        "ab",
        "Alice Smith",
        "alice1",
        "abcdefghijklmnopq",
        "álîce",
        "Kelvin",
    ] {
        alice.send(invalid);
        assert!(
            alice
                .until("Name: ")
                .contains("Names must contain 3-16 letters")
        );
    }
    alice.send("ALICE");
    let welcome = alice.until("alice> ");
    assert!(welcome.contains("Welcome, Alice!"));
    assert!(welcome.contains("The Common Room"));

    let mut bob = Client::connect(address);
    bob.send("alice");
    assert!(bob.until("Name: ").contains("already in use"));
    bob.send("Bob");
    bob.until("bob> ");
    alice.until("Bob has joined.\r\n");

    let mut unnamed = Client::connect(address);
    assert!(
        alice
            .command("alice", "who")
            .contains("Online: Alice, Bob.\r\n")
    );
    let literal = "%^RED%^<b>hello</b>";
    assert!(
        alice
            .command("alice", &format!("say {literal}"))
            .contains(&format!("You say: {literal}"))
    );
    bob.until(&format!("Alice says: {literal}\r\n"));
    assert!(
        bob.command("bob", "'Hello, Alice!")
            .contains("You say: Hello, Alice!")
    );
    alice.until("Bob says: Hello, Alice!\r\n");
    assert!(
        bob.command("bob", "emote waves.")
            .contains("Bob waves.\r\n")
    );
    alice.until("Bob waves.\r\n");
    assert!(
        alice
            .command("alice", "say")
            .contains("Please supply some text")
    );
    assert!(
        alice
            .command("alice", "say     ")
            .contains("Please supply some text")
    );
    let longest = "é".repeat(400);
    assert!(
        alice
            .command("alice", &format!("say {longest}"))
            .contains(&format!("You say: {longest}"))
    );
    bob.until(&format!("Alice says: {longest}\r\n"));
    assert!(
        alice
            .command("alice", &format!("say {}", "x".repeat(401)))
            .contains("400 characters or fewer")
    );
    assert!(
        alice
            .command("alice", "say hello\x1b[31m")
            .contains("without control characters")
    );
    assert!(
        alice
            .command("alice", "say one\u{2028}two")
            .contains("without control characters")
    );
    assert!(
        alice
            .command("alice", "emote")
            .contains("Please supply some text")
    );
    assert!(alice.command("alice", "look").contains("The Common Room"));
    let help = alice.command("alice", "help");
    for command in ["look", "who", "say", "emote", "help", "quit"] {
        assert!(help.contains(command));
    }
    assert!(
        alice
            .command("alice", "unknown")
            .contains("Unknown command. Type help")
    );
    assert!(
        !bob.command("bob", "who").contains("Alice says:"),
        "invalid chat leaked to Bob"
    );

    unnamed.send("quit");
    let unnamed_output = unnamed.eof();
    assert!(unnamed_output.contains("Goodbye!"));
    assert!(!unnamed_output.contains("says:") && !unnamed_output.contains("waves."));
    drop(Client::connect(address));

    drop(bob);
    alice.until("Bob has left.\r\n");
    assert!(alice.command("alice", "who").contains("Online: Alice.\r\n"));
    let mut bob = Client::connect(address);
    bob.send("bob");
    bob.until("bob> ");
    alice.until("Bob has joined.\r\n");
    bob.send("quit");
    assert!(bob.eof().contains("Goodbye, Bob!"));
    alice.until("Bob has left.\r\n");

    let mut first = Client::connect(address);
    let mut second = Client::connect(address);
    first.send("carol");
    second.send("CAROL");
    let first_reply = first.until("\r\n");
    let second_reply = second.until("\r\n");
    let first_won = first_reply.contains("Welcome, Carol!");
    assert_ne!(first_won, second_reply.contains("Welcome, Carol!"));
    assert!(if first_won { second_reply } else { first_reply }.contains("already in use"));
    let (winner, loser) = if first_won {
        (&mut first, &mut second)
    } else {
        (&mut second, &mut first)
    };
    winner.until("carol> ");
    loser.until("Name: ");
    assert!(
        alice
            .command("alice", "who")
            .contains("Online: Alice, Carol.\r\n")
    );
    winner.send("quit");
    winner.eof();
    alice.until("Carol has left.\r\n");
    loser.send("carol");
    loser.until("carol> ");

    driver.shutdown();
    assert!(
        alice
            .eof()
            .contains("The server is shutting down. Goodbye!")
    );
    assert!(
        loser
            .eof()
            .contains("The server is shutting down. Goodbye!")
    );
    let log = fs::read_to_string(&driver.log).unwrap();
    assert!(!log.contains(" ERROR "), "{log}");
}
