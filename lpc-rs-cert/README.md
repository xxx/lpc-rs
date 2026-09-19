# lpc-rs-cert

Obtain, renew, and install certificates for the lpc-rs TLS listener using Certbot.

```sh
lpc-rs-cert --env /etc/lpc-rs/mud.env ensure
lpc-rs-cert --env /etc/lpc-rs/mud.env status
lpc-rs-cert --env /etc/lpc-rs/mud.env ensure --dry-run
```

`ensure` requires an installed `certbot` executable; use `--certbot /absolute/path`
to select it. `status` works offline. The helper never changes the firewall or
installs scheduled jobs. See [secure connections](../doc/secure-connections.md)
for prerequisites, configuration, permissions, and a renewal timer.
