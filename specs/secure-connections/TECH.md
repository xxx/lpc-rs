# Secure connection implementation

## Context

At `69429ea6b84b058289e7f2a5be8e841c9662ccc7`, `src/telnet/mod.rs`
accepts TCP streams and runs a generic `AsyncRead + AsyncWrite` connection loop.
`Vm::boot` starts its listener; `ConfigBuilder` loads dotenv settings. See
[PRODUCT.md](PRODUCT.md) for the approved feature's observable behavior.

## Proposed changes

- Add shared environment reading and TLS settings/identity validation in
  `lpc-rs-utils`. Read dotenv files without mutating the process environment and
  return configuration errors to both existing binaries.
- Use rustls with Tokio's TLS adapter. Bind configured listeners before spawning
  acceptors. Bound concurrent handshakes and their duration; own acceptor and
  reload tasks through shutdown/drop. Flush buffered TLS output in the existing
  select loop without blocking incoming operations or idle deadlines.
- The TLS dependency requires newer `subtle`; Cargo resolves the legacy `pwhash`
  dependency's `crypto-mac` to 0.10.0 because 0.10.1 only adds an incompatible
  `subtle =2.4` pin. Password-hashing regression tests remain in the workspace gate.
- Validate PEM, key matching, validity, and helper hostname before activation.
  Reload into an immutable identity shared with new handshakes, retaining the
  previous identity on error. Read managed certificate/key material from one file.
- Add a workspace binary `lpc-rs-cert` that invokes Certbot with structured process
  arguments and dedicated config/work/log directories under `TLS_CERT_DIR`.
  Existing lineages use `renew`; initial issuance uses `certonly`. Certbot remains
  responsible for ACME and renewal policy. No shell commands or deploy hooks are
  generated. A file lock covers issuance through bundle publication.
- Publish a validated bundle using a private temporary file and atomic rename;
  new files are owner-only on Unix. Operators using separate service users must
  provision access explicitly. Keep secrets outside the canonical mudlib root.
- Document externally managed PEM files, HTTP-01 prerequisites, dry-run, service
  account permissions, timer setup, and failure recovery.

## Testing and validation

- Configuration tests cover defaults, precedence, invalid ports, incomplete TLS
  settings, disabled plaintext, and explicit file errors (behavior 1, 3, 10).
- Generated local certificates exercise validity, key mismatch, hostname checks,
  replacement, and rejection without public ACME requests (behavior 3–6, 9).
- Real loopback TLS clients verify Telnet traffic, malformed handshakes, reload,
  established-connection survival, and shutdown; buffered-stream tests cover flush
  behavior (behavior 2, 4, 5).
- Helper tests use a fake Certbot executable to exercise initial issuance,
  renewal, failure, dry-run, locking, and permissions (behavior 6–9).
- Run workspace build and tests, all-target clippy, formatting, and rustdoc with
  warnings denied. Live Let's Encrypt issuance requires the operator's domain and
  public challenge routing and is not part of automated tests.
