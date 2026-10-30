use std::{fs, io::Write, process::Command};

use rcgen::generate_simple_self_signed;

fn status_command() -> Command {
    let mut command = Command::new(env!("CARGO_BIN_EXE_lpc-rs-cert"));
    command.env_clear().arg("status");
    command
}

#[test]
fn status_requires_configuration() {
    let output = status_command().output().unwrap();
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("TLS_DOMAIN is required"), "{stderr}");
}

#[test]
fn status_reports_a_valid_local_certificate_without_certbot() {
    let root = tempfile::tempdir().unwrap();
    let certificates = root.path().join("certificates");
    let mudlib = root.path().join("mudlib");
    fs::create_dir_all(&certificates).unwrap();
    fs::create_dir_all(&mudlib).unwrap();
    let generated = generate_simple_self_signed(vec!["mud.example.org".into()]).unwrap();
    let mut bundle = tempfile::NamedTempFile::new_in(&certificates).unwrap();
    write!(
        bundle,
        "{}\n{}",
        generated.cert.pem(),
        generated.signing_key.serialize_pem()
    )
    .unwrap();
    bundle.persist(certificates.join("identity.pem")).unwrap();
    let output = status_command()
        .env("TLS_DOMAIN", "mud.example.org")
        .env("TLS_CERT_DIR", &certificates)
        .env("LIB_DIR", &mudlib)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    for expected in [
        "mud.example.org: valid TLS certificate",
        "Valid from:",
        "Expires:",
        "Bundle:",
    ] {
        assert!(stdout.contains(expected), "{stdout}");
    }
}
