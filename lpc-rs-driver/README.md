# lpc-rs-driver

This crate provides the `driver` binary for the `lpc-rs` project.

Configuration is entirely via environment variables. Load a `.env` file explicitly with
`-e <path>` or `--env <path>`; no `.env` file is loaded automatically.

See `default.env` for a commented example.

The driver supports a separate TLS listener and reloads certificates without
disconnecting players. See [secure connections](../doc/secure-connections.md)
for certificate setup and the `lpc-rs-cert` helper.

Please see the top-level [lpc-rs] documentation for more information.
