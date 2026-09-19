//! Shared TLS configuration and certificate validation for the driver and helper.

use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
    sync::Arc,
    time::{SystemTime, UNIX_EPOCH},
};

use lpc_rs_errors::{Result, lpc_error};
use rustls::{
    ServerConfig,
    pki_types::{CertificateDer, PrivateKeyDer, ServerName, pem::PemObject},
    server::ParsedCertificate,
};
use x509_parser::parse_x509_certificate;

/// Resolve the prefixed setting first, including an explicitly blank value.
pub fn setting<'a>(env: &'a HashMap<String, String>, name: &str) -> Option<&'a str> {
    env.get(&format!("LPC_{name}"))
        .or_else(|| env.get(name))
        .map(|s| s.trim())
}

/// Parse an optional boolean setting without silently accepting misspellings.
pub fn boolean_setting(env: &HashMap<String, String>, name: &str, default: bool) -> Result<bool> {
    match setting(env, name) {
        None => Ok(default),
        Some("true" | "1") => Ok(true),
        Some("false" | "0") => Ok(false),
        Some(_) => Err(lpc_error!("{name} must be true or false")),
    }
}

/// Certificate chain and matching private key, possibly in the same PEM file.
#[derive(Clone, Debug)]
pub struct CertificateFiles {
    pub certificate: PathBuf,
    pub private_key: PathBuf,
}

impl CertificateFiles {
    /// Use explicit PEM paths or the helper's single atomic bundle.
    pub fn from_env(env: &HashMap<String, String>) -> Result<Self> {
        let cert = setting(env, "TLS_CERT_FILE").filter(|v| !v.is_empty());
        let key = setting(env, "TLS_KEY_FILE").filter(|v| !v.is_empty());
        match (cert, key) {
            (Some(cert), Some(key)) => Ok(Self {
                certificate: cert.into(),
                private_key: key.into(),
            }),
            (None, None) => {
                let dir = setting(env, "TLS_CERT_DIR")
                    .filter(|v| !v.is_empty())
                    .ok_or_else(|| {
                        lpc_error!(
                            "TLS requires TLS_CERT_DIR or both TLS_CERT_FILE and TLS_KEY_FILE"
                        )
                    })?;
                let path = Path::new(dir).join("identity.pem");
                Ok(Self {
                    certificate: path.clone(),
                    private_key: path,
                })
            }
            _ => Err(lpc_error!(
                "TLS_CERT_FILE and TLS_KEY_FILE must be supplied together"
            )),
        }
    }

    /// Read the managed bundle once so its certificate and key cannot straddle a rename.
    pub fn load(&self) -> Result<Identity> {
        let cert = fs::read(&self.certificate).map_err(|e| {
            lpc_error!(
                "cannot read TLS certificate {}: {e}",
                self.certificate.display()
            )
        })?;
        if self.certificate == self.private_key {
            return Identity::from_pem(&cert, &cert);
        }
        let key = fs::read(&self.private_key).map_err(|e| {
            lpc_error!(
                "cannot read TLS private key {}: {e}",
                self.private_key.display()
            )
        })?;
        Identity::from_pem(&cert, &key)
    }
}

/// An optional dedicated TLS listener.
#[derive(Clone, Debug)]
pub struct TlsConfig {
    pub port: u16,
    pub files: CertificateFiles,
}

impl TlsConfig {
    /// A missing or blank port disables TLS; malformed ports are errors.
    pub fn from_env(env: &HashMap<String, String>) -> Result<Option<Self>> {
        let Some(port) = setting(env, "TLS_PORT").filter(|v| !v.is_empty()) else {
            return Ok(None);
        };
        let port = port
            .parse::<u16>()
            .ok()
            .filter(|p| *p != 0)
            .ok_or_else(|| lpc_error!("TLS_PORT must be between 1 and 65535"))?;
        Ok(Some(Self {
            port,
            files: CertificateFiles::from_env(env)?,
        }))
    }
}

/// A validated identity; private key bytes are never included in diagnostics.
pub struct Identity {
    pub server_config: Arc<ServerConfig>,
    pub not_before: i64,
    pub not_after: i64,
    leaf: CertificateDer<'static>,
    chain: Vec<CertificateDer<'static>>,
}

impl Identity {
    /// Validate PEM, certificate lifetime, and the private key's correspondence to the leaf.
    pub fn from_pem(certificate: &[u8], private_key: &[u8]) -> Result<Self> {
        let chain = CertificateDer::pem_slice_iter(certificate)
            .collect::<std::result::Result<Vec<_>, _>>()
            .map_err(|e| lpc_error!("invalid TLS certificate PEM: {e}"))?;
        let leaf = chain
            .first()
            .ok_or_else(|| lpc_error!("TLS certificate chain is empty"))?
            .clone();
        for certificate in &chain {
            let (remaining, _) = parse_x509_certificate(certificate.as_ref())
                .map_err(|_| lpc_error!("invalid certificate in TLS chain"))?;
            if !remaining.is_empty() {
                return Err(lpc_error!("trailing data in TLS certificate"));
            }
        }
        let (_, parsed) = parse_x509_certificate(leaf.as_ref())
            .map_err(|_| lpc_error!("invalid TLS leaf certificate"))?;
        let not_before = parsed.validity().not_before.timestamp();
        let not_after = parsed.validity().not_after.timestamp();
        let key = PrivateKeyDer::from_pem_slice(private_key)
            .map_err(|e| lpc_error!("invalid TLS private key PEM: {e}"))?;
        let server_config =
            ServerConfig::builder_with_provider(Arc::new(rustls::crypto::ring::default_provider()))
                .with_safe_default_protocol_versions()
                .map_err(|e| lpc_error!("TLS protocol configuration: {e}"))?
                .with_no_client_auth()
                .with_single_cert(chain.clone(), key)
                .map_err(|e| lpc_error!("invalid TLS certificate/key pair: {e}"))?;
        let identity = Self {
            server_config: Arc::new(server_config),
            not_before,
            not_after,
            leaf,
            chain,
        };
        identity.check_validity()?;
        Ok(identity)
    }

    /// Reject expired and not-yet-valid identities, including ones retained after a failed reload.
    pub fn check_validity(&self) -> Result<()> {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map_err(|e| lpc_error!("cannot determine certificate validity: {e}"))?
            .as_secs() as i64;
        if now < self.not_before {
            return Err(lpc_error!("TLS certificate is not yet valid"));
        }
        if now >= self.not_after {
            return Err(lpc_error!("TLS certificate has expired"));
        }
        Ok(())
    }

    /// Check the SAN hostname using the same verifier used by TLS clients.
    pub fn check_hostname(&self, domain: &str) -> Result<()> {
        let name = ServerName::try_from(domain).map_err(|_| lpc_error!("invalid TLS_DOMAIN"))?;
        let parsed = ParsedCertificate::try_from(&self.leaf)
            .map_err(|e| lpc_error!("invalid TLS certificate: {e}"))?;
        rustls::client::verify_server_name(&parsed, &name)
            .map_err(|e| lpc_error!("TLS certificate does not cover {domain}: {e}"))
    }

    /// Detect changes to the served chain without retaining the original private-key PEM.
    pub fn same_certificate(&self, other: &Self) -> bool {
        self.chain == other.chain
    }
}

/// Reject private material inside the mudlib, resolving existing symlink ancestors.
pub fn outside_mudlib(path: &Path, lib_dir: &str) -> Result<()> {
    let root = fs::canonicalize(if lib_dir.is_empty() { "." } else { lib_dir })
        .map_err(|e| lpc_error!("cannot resolve LIB_DIR: {e}"))?;
    let mut existing = path.to_path_buf();
    let mut suffix = Vec::new();
    while !existing.exists() {
        let name = existing
            .file_name()
            .ok_or_else(|| lpc_error!("invalid TLS path {}", path.display()))?
            .to_owned();
        suffix.push(name);
        existing.pop();
        if existing.as_os_str().is_empty() {
            existing.push(".");
        }
    }
    let mut resolved =
        fs::canonicalize(existing).map_err(|e| lpc_error!("cannot resolve TLS path: {e}"))?;
    for name in suffix.iter().rev() {
        resolved.push(name);
    }
    if resolved.starts_with(root) {
        return Err(lpc_error!("TLS private material must be outside LIB_DIR"));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use rcgen::{CertificateParams, KeyPair, date_time_ymd, generate_simple_self_signed};

    fn env(pairs: &[(&str, &str)]) -> HashMap<String, String> {
        pairs
            .iter()
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect()
    }

    #[test]
    fn tls_is_opt_in_and_prefixed_blank_disables_it() {
        assert!(TlsConfig::from_env(&env(&[])).unwrap().is_none());
        assert!(
            TlsConfig::from_env(&env(&[("TLS_PORT", "4040"), ("LPC_TLS_PORT", "")]))
                .unwrap()
                .is_none()
        );
    }

    #[test]
    fn tls_configuration_rejects_invalid_ports_and_incomplete_files() {
        for port in ["0", "65536", "oops", "-1"] {
            assert!(
                TlsConfig::from_env(&env(&[("TLS_PORT", port), ("TLS_CERT_DIR", "/tmp/tls")]))
                    .is_err()
            );
        }
        for pairs in [
            vec![("TLS_PORT", "4040")],
            vec![("TLS_PORT", "4040"), ("TLS_CERT_FILE", "/cert.pem")],
        ] {
            assert!(TlsConfig::from_env(&env(&pairs)).is_err());
        }
    }

    #[test]
    fn tls_paths_and_prefixed_settings_are_shared() {
        let config = TlsConfig::from_env(&env(&[
            ("TLS_PORT", "4040"),
            ("LPC_TLS_PORT", "4041"),
            ("TLS_CERT_DIR", "/var/lib/game/tls"),
        ]))
        .unwrap()
        .unwrap();
        assert_eq!(config.port, 4041);
        assert_eq!(
            config.files.certificate,
            Path::new("/var/lib/game/tls/identity.pem")
        );
        assert_eq!(config.files.private_key, config.files.certificate);
        let files = CertificateFiles::from_env(&env(&[
            ("TLS_CERT_FILE", "/fullchain.pem"),
            ("TLS_KEY_FILE", "/key.pem"),
        ]))
        .unwrap();
        assert_eq!(files.private_key, Path::new("/key.pem"));
    }

    #[test]
    fn identity_checks_hostname_key_match_and_pem() {
        let first = generate_simple_self_signed(vec!["mud.example.org".into()]).unwrap();
        let second = generate_simple_self_signed(vec!["mud.example.org".into()]).unwrap();
        let identity = Identity::from_pem(
            first.cert.pem().as_bytes(),
            first.signing_key.serialize_pem().as_bytes(),
        )
        .unwrap();
        identity.check_hostname("mud.example.org").unwrap();
        assert!(identity.check_hostname("wrong.example.org").is_err());
        assert!(
            Identity::from_pem(
                first.cert.pem().as_bytes(),
                second.signing_key.serialize_pem().as_bytes()
            )
            .is_err()
        );
        assert!(Identity::from_pem(b"not a certificate", b"not a key").is_err());
        let broken_chain = format!(
            "{}-----BEGIN CERTIFICATE-----\nAQ==\n-----END CERTIFICATE-----\n",
            first.cert.pem()
        );
        assert!(
            Identity::from_pem(
                broken_chain.as_bytes(),
                first.signing_key.serialize_pem().as_bytes()
            )
            .is_err()
        );
    }

    #[test]
    fn expired_and_future_certificates_are_rejected() {
        let key = KeyPair::generate().unwrap();
        for (start, end) in [(2000, 2001), (4000, 4001)] {
            let mut params = CertificateParams::new(vec!["mud.example.org".into()]).unwrap();
            params.not_before = date_time_ymd(start, 1, 1);
            params.not_after = date_time_ymd(end, 1, 1);
            let cert = params.self_signed(&key).unwrap();
            assert!(
                Identity::from_pem(cert.pem().as_bytes(), key.serialize_pem().as_bytes()).is_err()
            );
        }
    }

    #[test]
    fn bundled_and_separate_files_load_the_same_identity() {
        let dir = tempfile::tempdir().unwrap();
        let generated = generate_simple_self_signed(vec!["localhost".into()]).unwrap();
        let certificate = dir.path().join("chain.pem");
        let key = dir.path().join("key.pem");
        fs::write(&certificate, generated.cert.pem()).unwrap();
        fs::write(&key, generated.signing_key.serialize_pem()).unwrap();
        let separate = CertificateFiles {
            certificate: certificate.clone(),
            private_key: key,
        }
        .load()
        .unwrap();
        fs::write(
            &certificate,
            format!(
                "{}{}",
                generated.cert.pem(),
                generated.signing_key.serialize_pem()
            ),
        )
        .unwrap();
        let bundled = CertificateFiles {
            certificate: certificate.clone(),
            private_key: certificate,
        }
        .load()
        .unwrap();
        assert!(bundled.same_certificate(&separate));
    }

    #[test]
    fn keys_cannot_be_placed_beneath_the_mudlib() {
        let dir = tempfile::tempdir().unwrap();
        let lib = dir.path().join("lib");
        fs::create_dir(&lib).unwrap();
        assert!(outside_mudlib(&lib.join("new/tls/identity.pem"), lib.to_str().unwrap()).is_err());
        outside_mudlib(&dir.path().join("tls/identity.pem"), lib.to_str().unwrap()).unwrap();
        #[cfg(unix)]
        {
            let link = dir.path().join("link");
            std::os::unix::fs::symlink(&lib, &link).unwrap();
            assert!(outside_mudlib(&link.join("identity.pem"), lib.to_str().unwrap()).is_err());
        }
    }
}
