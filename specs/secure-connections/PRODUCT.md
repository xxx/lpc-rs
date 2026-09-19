# Secure player connections

## Summary

Players can connect using TLS. Operators can supply certificates themselves or
use `lpc-rs-cert` with the driver's environment file to obtain, renew, and install
Let's Encrypt certificates.

## Behavior

1. Existing configurations keep their plaintext listener. `TLS_PORT` enables a
   separate TLS listener; `TELNET_ENABLED=false` disables plaintext. A driver
   configured without either listener fails startup.
2. TLS connections use the same login, Telnet negotiation, commands, and shutdown
   behavior as plaintext connections. TLS handshakes are bounded and cannot block
   other players from connecting. Plaintext sent to the TLS port is rejected.
3. `TLS_CERT_DIR` selects a managed `identity.pem` containing the certificate chain
   and private key. Alternatively, `TLS_CERT_FILE` and `TLS_KEY_FILE` select
   externally managed PEM files. Paths are host paths, relative to the working
   directory when not absolute; private material belongs outside `LIB_DIR`.
4. Enabled TLS with missing, malformed, expired, not-yet-valid, or mismatched
   certificate/key material fails startup. Failed reloads retain the last valid
   identity and log an error; new handshakes stop if that identity expires.
5. The driver checks certificates every minute. A successful replacement affects
   new connections without interrupting established players. Shutdown stops both
   listeners and any incomplete handshakes.
6. `lpc-rs-cert --env mud.env ensure` uses installed Certbot to obtain or renew the
   single hostname in `TLS_DOMAIN`, then validates and atomically installs the
   resulting bundle in `TLS_CERT_DIR`. Concurrent helper runs are rejected. Failed
   issuance or validation leaves the installed bundle untouched.
7. The helper supports HTTP-01 using Certbot's standalone server, or an existing
   web server when `ACME_WEBROOT` is supplied. Public port 80 must reach the
   challenge server on every issuance and renewal. DNS-01 is supported through
   externally managed certificates, not the helper.
8. Initial issuance requires `ACME_EMAIL` and explicit `ACME_AGREE_TOS=true`.
   Certbot owns ACME accounts, renewal timing, and protocol retries. A documented
   systemd timer runs `ensure` twice daily with jitter; the helper does not install
   services or change firewall rules.
9. `status` validates the installed identity and reports its hostname and validity
   without contacting ACME or requiring Certbot. Missing or invalid material exits
   unsuccessfully. `ensure --dry-run` exercises validation against staging without
   installing a staging certificate or replacing the active bundle.
10. Driver and helper use the same environment-file precedence: process variables
    override the same file variables; `LPC_` names override unprefixed names.
    An explicitly requested unreadable or malformed file is an error.
