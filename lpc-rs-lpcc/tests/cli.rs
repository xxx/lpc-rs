use std::{fs, path::Path, process::Command};

fn lpcc(root: &Path) -> Command {
    let mut command = Command::new(env!("CARGO_BIN_EXE_lpc-rs-lpcc"));
    command
        .env_clear()
        .env("LPC_LIB_DIR", root)
        .current_dir(root);
    command
}

#[test]
fn both_assembly_flags_emit_instructions_without_running_initializers() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(
        dir.path().join("object.c"),
        r#"
int value = fail();
int fail() { throw("global initializer ran"); return 0; }
void create() { throw("create ran"); }
int answer() { return 42; }
"#,
    )
    .unwrap();

    for flag in ["--emit-asm", "-S"] {
        let output = lpcc(dir.path()).args([flag, "object.c"]).output().unwrap();
        assert!(output.status.success(), "{output:?}");
        let listing = String::from_utf8_lossy(&output.stdout);
        assert!(listing.contains("fn answer ("), "{listing}");
        assert!(listing.contains("fn create ("), "{listing}");
        assert!(listing.contains("k0 = 42"), "{listing}");
        assert!(listing.contains("copy k0, r0"), "{listing}");
        assert!(output.stderr.is_empty(), "{output:?}");
    }
}

#[test]
fn assembly_resolves_configured_dependencies_without_initializing_them() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("auto.h"), "#define VALUE 3\n").unwrap();
    fs::write(
        dir.path().join("base.c"),
        "int inherited() { return 2; }\nvoid create() { throw(\"base ran\"); }\n",
    )
    .unwrap();
    fs::write(
        dir.path().join("sefun.c"),
        "nomask int helper() { return 1; }\nvoid create() { throw(\"sefun ran\"); }\n",
    )
    .unwrap();
    fs::write(
        dir.path().join("object.c"),
        "int answer() { return helper() + inherited() + VALUE; }\n",
    )
    .unwrap();
    fs::write(
        dir.path().join("compiler.env"),
        "AUTO_INCLUDE_FILE=/auto.h\nAUTO_INHERIT_FILE=/base\nSIMUL_EFUN_FILE=/sefun\n",
    )
    .unwrap();

    let output = lpcc(dir.path())
        .args(["-S", "--config", "compiler.env", "object"])
        .output()
        .unwrap();
    assert!(output.status.success(), "{output:?}");
    let listing = String::from_utf8_lossy(&output.stdout);
    assert!(listing.contains("fn answer ("), "{listing}");
    assert!(listing.contains("fn inherited ("), "{listing}");
    assert!(!listing.contains("fn helper ("), "{listing}");
    assert!(output.stderr.is_empty(), "{output:?}");
}

#[test]
fn assembly_can_target_the_configured_simul_efun_file() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(
        dir.path().join("sefun.c"),
        "nomask int helper() { return 1; }\nvoid create() { throw(\"sefun ran\"); }\n",
    )
    .unwrap();

    let output = lpcc(dir.path())
        .env("LPC_SIMUL_EFUN_FILE", "/sefun")
        .args(["-S", "sefun.c"])
        .output()
        .unwrap();
    assert!(output.status.success(), "{output:?}");
    assert!(String::from_utf8_lossy(&output.stdout).contains("fn helper ("));
    assert!(output.stderr.is_empty(), "{output:?}");
}

#[test]
fn assembly_warnings_go_to_stderr() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("object.c"), "void f() { int unused; }\n").unwrap();

    let output = lpcc(dir.path()).args(["-S", "object.c"]).output().unwrap();
    assert!(output.status.success(), "{output:?}");
    let listing = String::from_utf8_lossy(&output.stdout);
    let diagnostics = String::from_utf8_lossy(&output.stderr);
    assert!(listing.contains("fn f ("), "{listing}");
    assert!(!listing.contains("unused variable"), "{listing}");
    assert!(
        diagnostics.contains("unused variable `unused`"),
        "{diagnostics}"
    );
}

#[test]
fn assembly_errors_fail_without_printing_a_listing_or_panicking() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(
        dir.path().join("object.c"),
        "int f() { return absent(); }\n",
    )
    .unwrap();

    let output = lpcc(dir.path()).args(["-S", "object.c"]).output().unwrap();
    assert_eq!(output.status.code(), Some(1), "{output:?}");
    assert!(output.stdout.is_empty(), "{output:?}");
    let diagnostics = String::from_utf8_lossy(&output.stderr);
    assert!(diagnostics.contains("absent"), "{diagnostics}");
    assert!(!diagnostics.contains("panicked"), "{diagnostics}");
}

#[test]
fn assembly_and_check_are_mutually_exclusive() {
    let dir = tempfile::tempdir().unwrap();
    let output = lpcc(dir.path())
        .args(["--emit-asm", "--check", "object.c"])
        .output()
        .unwrap();
    assert_eq!(output.status.code(), Some(2), "{output:?}");
    assert!(output.stdout.is_empty(), "{output:?}");
    let diagnostics = String::from_utf8_lossy(&output.stderr);
    assert!(diagnostics.contains("cannot be used with"), "{diagnostics}");
}

#[test]
fn execution_runs_create_while_check_skips_it() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(
        dir.path().join("object.c"),
        "void create() { throw(\"create executed\"); }\n",
    )
    .unwrap();

    let output = lpcc(dir.path()).arg("object.c").output().unwrap();
    assert_eq!(output.status.code(), Some(1), "{output:?}");
    assert!(String::from_utf8_lossy(&output.stderr).contains("create executed"));

    let output = lpcc(dir.path())
        .args(["--check", "object.c"])
        .output()
        .unwrap();
    assert!(output.status.success(), "{output:?}");
    assert!(output.stdout.is_empty(), "{output:?}");
}
