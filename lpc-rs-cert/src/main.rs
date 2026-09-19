//! Certificate lifecycle helper; Certbot owns ACME, and the driver consumes an atomic PEM bundle.
#![forbid(unsafe_code)]

use std::{
    collections::HashMap,
    fs::{self, File, OpenOptions},
    io::Write,
    path::{Path, PathBuf},
    process::{Command, ExitCode},
};

use anyhow::{Context, Result, bail};
use clap::{Parser, Subcommand};
use lpc_rs_utils::{
    config::read_env,
    tls::{CertificateFiles, Identity, boolean_setting, outside_mudlib, setting},
};

#[derive(Parser)]
#[command(
    version,
    about = "Obtain, renew, and install TLS certificates for lpc-rs"
)]
struct Args {
    /// Read the same dotenv configuration as the driver.
    #[arg(short, long, global = true)]
    env: Option<PathBuf>,
    /// Certbot executable (required only for ensure).
    #[arg(long, default_value = "certbot", global = true)]
    certbot: PathBuf,
    #[command(subcommand)]
    command: Action,
}

#[derive(Subcommand)]
enum Action {
    /// Obtain or renew when due, and install a validated certificate/key bundle.
    Ensure {
        /// Exercise ACME validation against staging without installing a certificate.
        #[arg(long)]
        dry_run: bool,
    },
    /// Validate the installed certificate and print its validity without network access.
    Status,
}

struct Settings {
    domain: String,
    directory: PathBuf,
    email: Option<String>,
    agree_tos: bool,
    webroot: Option<PathBuf>,
    lib_dir: String,
}

impl Settings {
    fn from_env(env: &HashMap<String, String>) -> Result<Self> {
        let required = |name| {
            setting(env, name)
                .filter(|v| !v.is_empty())
                .with_context(|| format!("{name} is required"))
        };
        let domain = required("TLS_DOMAIN")?.to_ascii_lowercase();
        if domain.len() > 253
            || !domain.contains('.')
            || domain.split('.').any(|label| {
                label.is_empty()
                    || label.len() > 63
                    || label.starts_with('-')
                    || label.ends_with('-')
                    || !label
                        .bytes()
                        .all(|c| c.is_ascii_alphanumeric() || c == b'-')
            })
            || domain.parse::<std::net::IpAddr>().is_ok()
        {
            bail!("TLS_DOMAIN must be one DNS hostname (use ASCII/punycode; no wildcards)");
        }
        if setting(env, "ACME_CHALLENGE").is_some_and(|v| v != "http-01") {
            bail!(
                "the helper supports ACME_CHALLENGE=http-01; use external PEM files for other methods"
            );
        }
        if ["TLS_CERT_FILE", "TLS_KEY_FILE"]
            .iter()
            .any(|key| setting(env, key).is_some_and(|v| !v.is_empty()))
        {
            bail!(
                "the helper manages TLS_CERT_DIR; remove TLS_CERT_FILE and TLS_KEY_FILE overrides"
            );
        }
        let directory = std::path::absolute(required("TLS_CERT_DIR")?)?;
        let lib_dir = setting(env, "LIB_DIR").unwrap_or_default().to_owned();
        outside_mudlib(&directory, &lib_dir)?;
        Ok(Self {
            domain,
            directory,
            lib_dir,
            email: setting(env, "ACME_EMAIL")
                .filter(|v| !v.is_empty())
                .map(str::to_owned),
            agree_tos: boolean_setting(env, "ACME_AGREE_TOS", false)?,
            webroot: setting(env, "ACME_WEBROOT")
                .filter(|v| !v.is_empty())
                .map(std::path::absolute)
                .transpose()?,
        })
    }

    fn installed(&self) -> CertificateFiles {
        let path = self.directory.join("identity.pem");
        CertificateFiles {
            certificate: path.clone(),
            private_key: path,
        }
    }

    fn issued(&self) -> CertificateFiles {
        let live = self.directory.join("certbot/live").join(&self.domain);
        CertificateFiles {
            certificate: live.join("fullchain.pem"),
            private_key: live.join("privkey.pem"),
        }
    }
}

fn main() -> ExitCode {
    match run(Args::parse()) {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            eprintln!("lpc-rs-cert: {error:#}");
            ExitCode::FAILURE
        }
    }
}

fn run(args: Args) -> Result<()> {
    let settings = Settings::from_env(&read_env(args.env)?)?;
    match args.command {
        Action::Ensure { dry_run } => ensure(&settings, &args.certbot, dry_run),
        Action::Status => status(&settings),
    }
}

fn status(settings: &Settings) -> Result<()> {
    let identity = settings.installed().load()?;
    identity.check_hostname(&settings.domain)?;
    println!("{}: valid TLS certificate", settings.domain);
    for (label, seconds) in [
        ("Valid from", identity.not_before),
        ("Expires", identity.not_after),
    ] {
        let date = chrono::DateTime::from_timestamp(seconds, 0)
            .context("certificate date is out of range")?;
        println!("{label}: {}", date.to_rfc3339());
    }
    println!("Bundle: {}", settings.installed().certificate.display());
    Ok(())
}

fn private_directory(path: &Path) -> Result<()> {
    let mut builder = fs::DirBuilder::new();
    builder.recursive(true);
    #[cfg(unix)]
    {
        use std::os::unix::fs::DirBuilderExt;
        builder.mode(0o700);
    }
    builder
        .create(path)
        .with_context(|| format!("cannot create {}", path.display()))
}

fn lock(directory: &Path) -> Result<File> {
    let mut options = OpenOptions::new();
    options.read(true).write(true).create(true).truncate(false);
    #[cfg(unix)]
    {
        use std::os::unix::fs::OpenOptionsExt;
        options.mode(0o600);
    }
    let file = options.open(directory.join(".helper.lock"))?;
    file.try_lock()
        .context("certificate helper is already running, or its state cannot be locked")?;
    Ok(file)
}

fn ensure(settings: &Settings, executable: &Path, dry_run: bool) -> Result<()> {
    private_directory(&settings.directory)?;
    outside_mudlib(&settings.directory, &settings.lib_dir)?;
    let _lock = lock(&settings.directory)?;
    let renewal = settings
        .directory
        .join("certbot/renewal")
        .join(format!("{}.conf", settings.domain));
    let existing = renewal.try_exists()?;
    if !existing && (!settings.agree_tos || settings.email.is_none()) {
        bail!(
            "initial issuance requires ACME_EMAIL and ACME_AGREE_TOS=true (accept Let's Encrypt's subscriber agreement)"
        );
    }

    let mut command = Command::new(executable);
    command.arg(if existing { "renew" } else { "certonly" });
    command.args(["--non-interactive", "--cert-name", &settings.domain]);
    for (flag, subdir) in [
        ("--config-dir", "certbot"),
        ("--work-dir", "work"),
        ("--logs-dir", "logs"),
    ] {
        let dir = settings.directory.join(subdir);
        private_directory(&dir)?;
        command.arg(flag).arg(dir);
    }
    // An explicit empty config isolates the helper from system-wide Certbot options.
    let empty_config = tempfile::NamedTempFile::new_in(&settings.directory)?;
    command.arg("--config").arg(empty_config.path());
    command.args([
        "--server",
        if dry_run {
            "https://acme-staging-v02.api.letsencrypt.org/directory"
        } else {
            "https://acme-v02.api.letsencrypt.org/directory"
        },
    ]);
    if dry_run {
        command.arg("--dry-run");
    }
    if !existing {
        command.args(["--domain", &settings.domain]);
    }
    if settings.agree_tos {
        command.arg("--agree-tos");
    }
    if let Some(email) = &settings.email {
        command.args(["--email", email]);
    }
    command.args(["--preferred-challenges", "http"]);
    if let Some(webroot) = &settings.webroot {
        command.arg("--webroot").arg("--webroot-path").arg(webroot);
    } else {
        command.arg("--standalone");
    }
    let result = command.status().with_context(|| {
        format!(
            "cannot run {}; install Certbot or use --certbot /path/to/certbot",
            executable.display()
        )
    })?;
    if !result.success() {
        bail!("Certbot exited with {result}; the installed TLS bundle was not changed");
    }
    if dry_run {
        println!("ACME dry run succeeded; the installed TLS bundle was not changed");
        return Ok(());
    }
    publish(settings)?;
    status(settings)
}

fn publish(settings: &Settings) -> Result<()> {
    let issued = settings.issued();
    let mut pem =
        fs::read(&issued.certificate).context("Certbot did not produce a certificate chain")?;
    pem.push(b'\n');
    pem.extend(fs::read(&issued.private_key).context("Certbot did not produce a private key")?);
    let identity = Identity::from_pem(&pem, &pem)?;
    identity.check_hostname(&settings.domain)?;
    let destination = settings.installed().certificate;
    if fs::read(&destination).is_ok_and(|installed| installed == pem) {
        println!("Installed certificate is unchanged");
        return Ok(());
    }
    let mut file = tempfile::NamedTempFile::new_in(&settings.directory)?;
    file.write_all(&pem)?;
    file.as_file().sync_all()?;
    file.persist(&destination)
        .context("cannot atomically install TLS bundle")?;
    #[cfg(unix)]
    File::open(&settings.directory)?.sync_all()?;
    println!("Installed certificate; the driver will reload it within one minute");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use rcgen::generate_simple_self_signed;

    fn settings(directory: &Path) -> Settings {
        Settings {
            domain: "mud.example.org".into(),
            directory: directory.to_owned(),
            email: Some("operator@example.org".into()),
            agree_tos: true,
            webroot: None,
            lib_dir: String::new(),
        }
    }

    fn issued(settings: &Settings, domain: &str) {
        let generated = generate_simple_self_signed(vec![domain.into()]).unwrap();
        let files = settings.issued();
        fs::create_dir_all(files.certificate.parent().unwrap()).unwrap();
        fs::write(files.certificate, generated.cert.pem()).unwrap();
        fs::write(files.private_key, generated.signing_key.serialize_pem()).unwrap();
    }

    #[test]
    fn publication_preserves_the_last_certificate_on_invalid_replacement() {
        let dir = tempfile::tempdir().unwrap();
        let settings = settings(dir.path());
        issued(&settings, &settings.domain);
        publish(&settings).unwrap();
        let original = fs::read(settings.installed().certificate).unwrap();
        issued(&settings, "another.example.org");
        assert!(publish(&settings).is_err());
        assert_eq!(
            fs::read(settings.installed().certificate).unwrap(),
            original
        );
        issued(&settings, &settings.domain);
        fs::write(settings.issued().private_key, "broken key").unwrap();
        assert!(publish(&settings).is_err());
        assert_eq!(
            fs::read(settings.installed().certificate).unwrap(),
            original
        );
    }

    #[test]
    fn helper_locks_cover_the_entire_operation() {
        let dir = tempfile::tempdir().unwrap();
        let first = lock(dir.path()).unwrap();
        assert!(lock(dir.path()).is_err());
        drop(first);
        lock(dir.path()).unwrap();
    }

    #[test]
    fn initial_issuance_requires_explicit_terms_acceptance() {
        let dir = tempfile::tempdir().unwrap();
        let mut settings = settings(dir.path());
        settings.agree_tos = false;
        let error = ensure(&settings, Path::new("must-not-run-certbot"), false).unwrap_err();
        assert!(error.to_string().contains("ACME_AGREE_TOS"));
    }

    #[test]
    fn helper_rejects_domain_paths_and_conflicting_certificate_settings() {
        let dir = tempfile::tempdir().unwrap();
        let mut env = HashMap::from([
            ("TLS_DOMAIN".into(), "mud.example.org".into()),
            ("TLS_CERT_DIR".into(), dir.path().to_str().unwrap().into()),
        ]);
        Settings::from_env(&env).unwrap();
        for domain in [
            "../escape",
            "*.example.org",
            "127.0.0.1",
            "--flag",
            "a..org",
            "-a.example.org",
        ] {
            env.insert("TLS_DOMAIN".into(), domain.into());
            assert!(Settings::from_env(&env).is_err(), "{domain}");
        }
        env.insert("TLS_DOMAIN".into(), "mud.example.org".into());
        env.insert("TLS_KEY_FILE".into(), "/some/key".into());
        assert!(Settings::from_env(&env).is_err());
    }

    #[cfg(unix)]
    #[test]
    fn ensure_runs_initial_issuance_then_renewal_and_never_publishes_a_dry_run() {
        use std::os::unix::fs::PermissionsExt;
        let dir = tempfile::tempdir().unwrap();
        let settings = settings(&dir.path().join("state with spaces"));
        let executable = dir.path().join("fake certbot");
        fs::write(
            &executable,
            r##"#!/bin/sh
base=$(dirname "$0")
printf '%s\n' "$@" > "$base/arguments"
test ! -e "$base/fail" || exit 19
while test "$#" -gt 0; do
    case "$1" in
        --config-dir) config=$2; shift ;;
        --cert-name) name=$2; shift ;;
    esac
    shift
done
mkdir -p "$config/renewal" "$config/live/$name"
touch "$config/renewal/$name.conf"
cp "$base/chain.pem" "$config/live/$name/fullchain.pem"
cp "$base/key.pem" "$config/live/$name/privkey.pem"
"##,
        )
        .unwrap();
        fs::set_permissions(&executable, fs::Permissions::from_mode(0o700)).unwrap();
        let fixture = generate_simple_self_signed(vec![settings.domain.clone()]).unwrap();
        fs::write(dir.path().join("chain.pem"), fixture.cert.pem()).unwrap();
        fs::write(
            dir.path().join("key.pem"),
            fixture.signing_key.serialize_pem(),
        )
        .unwrap();
        ensure(&settings, &executable, false).unwrap();
        let arguments = fs::read_to_string(dir.path().join("arguments")).unwrap();
        assert!(arguments.starts_with("certonly\n"));
        assert!(arguments.contains("--standalone\n"));
        let bundle = settings.installed().certificate;
        assert_eq!(
            fs::metadata(&bundle).unwrap().permissions().mode() & 0o777,
            0o600
        );
        assert_eq!(
            fs::metadata(&settings.directory)
                .unwrap()
                .permissions()
                .mode()
                & 0o777,
            0o700
        );
        let original = fs::read(&bundle).unwrap();
        ensure(&settings, &executable, false).unwrap();
        assert!(
            fs::read_to_string(dir.path().join("arguments"))
                .unwrap()
                .starts_with("renew\n")
        );
        assert!(
            fs::read_to_string(dir.path().join("arguments"))
                .unwrap()
                .contains("--agree-tos\n--email\noperator@example.org\n")
        );
        assert_eq!(fs::read(&bundle).unwrap(), original);
        let replacement = generate_simple_self_signed(vec![settings.domain.clone()]).unwrap();
        fs::write(dir.path().join("chain.pem"), replacement.cert.pem()).unwrap();
        fs::write(
            dir.path().join("key.pem"),
            replacement.signing_key.serialize_pem(),
        )
        .unwrap();
        ensure(&settings, &executable, true).unwrap();
        assert_eq!(fs::read(&bundle).unwrap(), original);
        assert!(
            fs::read_to_string(dir.path().join("arguments"))
                .unwrap()
                .contains("https://acme-staging-v02.api.letsencrypt.org/directory\n")
        );
        fs::write(dir.path().join("fail"), "").unwrap();
        assert!(ensure(&settings, &executable, false).is_err());
        assert_eq!(fs::read(&bundle).unwrap(), original);
        fs::remove_file(dir.path().join("fail")).unwrap();
        ensure(&settings, &executable, false).unwrap();
        assert_ne!(fs::read(&bundle).unwrap(), original);
        status(&settings).unwrap();
    }
}
