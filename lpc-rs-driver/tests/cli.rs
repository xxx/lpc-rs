#![cfg(unix)]

use std::{
    fs::{self, File},
    process::{Child, Command, Stdio},
    thread,
    time::{Duration, Instant},
};

struct DriverProcess(Child);

impl Drop for DriverProcess {
    fn drop(&mut self) {
        let _ = self.0.kill();
        let _ = self.0.wait();
    }
}

#[test]
fn shutdown_propagates_the_requested_nonzero_exit_code() {
    let directory = tempfile::tempdir().unwrap();
    let mudlib = directory.path().join("mudlib");
    fs::create_dir_all(mudlib.join("secure")).unwrap();
    fs::write(
        mudlib.join("secure/master.c"),
        r#"
            int valid_shutdown(object caller, string program) { return 1; }
            string *epilog(int load_empty) { shutdown(17); return ({}); }
        "#,
    )
    .unwrap();
    let log = directory.path().join("driver.log");
    let output = File::create(&log).unwrap();
    let child = Command::new(env!("CARGO_BIN_EXE_lpc-rs-driver"))
        .current_dir(&mudlib)
        .env_clear()
        .env("LPC_LIB_DIR", &mudlib)
        .env("LPC_MASTER_OBJECT", "/secure/master.c")
        .env("LPC_AUTO_INCLUDE_FILE", "")
        .env("LPC_AUTO_INHERIT_FILE", "")
        .env("LPC_SIMUL_EFUN_FILE", "")
        .env("LPC_BIND_ADDRESS", "127.0.0.1")
        .env("LPC_PORT", "0")
        .env("LPC_SERVER_LOG_FILE", "STDOUT")
        .env("LPC_DEBUG_LOG_FILE", "STDOUT")
        .stdin(Stdio::null())
        .stdout(output.try_clone().unwrap())
        .stderr(output)
        .spawn()
        .unwrap();
    let mut driver = DriverProcess(child);
    let deadline = Instant::now() + Duration::from_secs(15);
    let status = loop {
        if let Some(status) = driver.0.try_wait().unwrap() {
            break status;
        }
        assert!(
            Instant::now() < deadline,
            "driver did not exit: {}",
            fs::read_to_string(&log).unwrap()
        );
        thread::sleep(Duration::from_millis(20));
    };
    assert_eq!(
        status.code(),
        Some(17),
        "{}",
        fs::read_to_string(log).unwrap()
    );
}
