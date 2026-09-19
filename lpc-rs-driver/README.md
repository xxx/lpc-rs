# lpc-rs-driver

This crate provides the `lpc-rs-driver` binary for the `lpc-rs` project.

For a runnable starting point, copy [ulib](../ulib/README.md) from the source
distribution, change into that directory and run:

```sh
lpc-rs-driver --env driver.env
```

ulib provides temporary guest names and one shared chat room, with documented
source and examples of every apply. Installing the driver binary does not copy
the mudlib; its guide includes installation and first-connection instructions.

Configuration is entirely via environment variables. Load a `.env` file explicitly with
`-e <path>` or `--env <path>`; no `.env` file is loaded automatically.

See [default.env](../default.env) for a commented example.

The driver supports a separate TLS listener and reloads certificates without
disconnecting players. See [secure connections](../doc/secure-connections.md)
for certificate setup and the `lpc-rs-cert` helper.

See the top-level [lpc-rs documentation](../README.md) for more information.
