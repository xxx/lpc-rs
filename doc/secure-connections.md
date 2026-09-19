# Secure player connections

The driver accepts Telnet over TLS on a separate port. Players select TLS in their
MUD client and connect using the certificate's hostname. The protocol inside TLS
has the same login, commands, and Telnet extensions as the plaintext connection;
this is direct TLS, without STARTTLS negotiation.

## Obtain a certificate with the helper

Install a current [Certbot](https://certbot.eff.org/instructions) and build the
helper and driver with `cargo build --release --workspace`. The helper invokes
Certbot directly; `--certbot /absolute/path/to/certbot` selects another executable.

Add these settings to the same dotenv file used by the driver, for example
`/etc/lpc-rs/mud.env`:

```dotenv
LIB_DIR=/srv/my-mud/lib
PORT=24960
TLS_PORT=4040
TLS_CERT_DIR=/var/lib/lpc-rs/tls
TLS_DOMAIN=mud.example.org
ACME_EMAIL=operator@example.org
ACME_AGREE_TOS=true
ACME_CHALLENGE=http-01
```

Set `ACME_AGREE_TOS=true` only after accepting the
[Let's Encrypt subscriber agreement](https://letsencrypt.org/repository/).
Replace the example hostname and email with your own.

Point the hostname's public DNS records at the server. HTTP-01 validation must
reach **port 80**, including over IPv6 when the hostname has an AAAA record.
The MUD's TLS port can be any available port; it does not replace the validation
port. Permit connections to both in your firewall and any router forwarding.
See [Let's Encrypt's challenge requirements](https://letsencrypt.org/docs/challenge-types/).

By default, Certbot temporarily binds port 80 during issuance or renewal. Run the
helper under an account with permission to bind that port and write its state
directory. Use the same service account as the driver so it can read the bundle.
The systemd example below grants the port-binding capability without running the
game as root.

If an existing web server already uses port 80, set:

```dotenv
ACME_WEBROOT=/var/www/html
```

That server must serve `/.well-known/acme-challenge/` from this webroot for the
configured hostname, and the helper's service account must be able to write there.
For DNS validation or other ACME setups, use externally managed certificates as
described below.

First test validation, then obtain and inspect the certificate:

```sh
lpc-rs-cert --env /etc/lpc-rs/mud.env ensure --dry-run
lpc-rs-cert --env /etc/lpc-rs/mud.env ensure
lpc-rs-cert --env /etc/lpc-rs/mud.env status
lpc-rs-driver --env /etc/lpc-rs/mud.env
```

Dry runs use Let's Encrypt staging and never publish a staging certificate to the
driver. `status` checks the installed certificate, hostname, and matching key
without a network request. Errors return a nonzero exit status.

The helper creates `identity.pem` in `TLS_CERT_DIR`. It contains the chain and
private key together so replacements are atomic. On Unix new bundles have mode
`0600` and new state directories have mode `0700`. Keep the entire directory,
including Certbot's account state, outside `LIB_DIR`; the helper and driver reject
private material beneath the mudlib, including through existing symlinks. Running
the helper and driver as different users requires an operator-managed publication
and permissions arrangement; use external PEM paths for that setup.

All TLS paths are host paths, relative to the working directory when not absolute.
Use absolute paths in service configurations. Both programs read an explicitly
selected dotenv file without exporting it into the process environment. Process
variables override file values with the same name; `LPC_`-prefixed settings take
precedence over unprefixed settings. Missing or malformed explicit files are errors.

## Schedule renewal

`ensure` obtains the first certificate, then uses Certbot's `renew` command for
its saved lineage on subsequent invocations. Certbot decides when renewal is due;
the helper does not assume a fixed certificate lifetime. Preserve `TLS_CERT_DIR`
across restarts and container replacements to retain ACME accounts and certificates.

Install the built helper as `/usr/local/bin/lpc-rs-cert`, and adapt this service to
the account that runs your driver. For `/etc/systemd/system/lpc-rs-cert.service`:

```ini
[Unit]
Description=Renew the lpc-rs TLS certificate
Wants=network-online.target
After=network-online.target

[Service]
Type=oneshot
User=lpc-rs
Group=lpc-rs
StateDirectory=lpc-rs
WorkingDirectory=/var/lib/lpc-rs
AmbientCapabilities=CAP_NET_BIND_SERVICE
CapabilityBoundingSet=CAP_NET_BIND_SERVICE
ExecStart=/usr/local/bin/lpc-rs-cert --env /etc/lpc-rs/mud.env ensure
```

For `/etc/systemd/system/lpc-rs-cert.timer`:

```ini
[Unit]
Description=Check the lpc-rs TLS certificate twice daily

[Timer]
OnCalendar=*-*-* 00,12:00:00
RandomizedDelaySec=1h
Persistent=true

[Install]
WantedBy=timers.target
```

Activate it and perform the first issuance under that service account:

```sh
sudo systemctl daemon-reload
sudo systemctl start lpc-rs-cert.service
sudo systemctl enable --now lpc-rs-cert.timer
```

Monitor failures with `journalctl -u lpc-rs-cert.service`. Fix DNS, port access,
permissions, or Certbot errors and run the service again. A failed attempt leaves
the active bundle unchanged. Renewal state lives under this helper's directory,
so a system-wide Certbot timer alone does not renew or publish this certificate.
Concurrent helper runs on the same directory are rejected.

## Supply an existing certificate

The driver can load any compatible PEM chain and private key:

```dotenv
TLS_PORT=4040
TLS_CERT_FILE=/etc/letsencrypt/live/mud.example.org/fullchain.pem
TLS_KEY_FILE=/etc/letsencrypt/live/mud.example.org/privkey.pem
```

Provide both paths, make them readable by the driver account, and manage renewal
with your chosen tool. These settings override `TLS_CERT_DIR` in the driver; the
helper rejects them to avoid installing a bundle that the driver would ignore.
Include the intermediate certificates in the chain. The driver validates PEM,
validity, and the matching key; clients validate trust and the hostname.

The driver reads the files every minute. A valid replacement is used for new
connections while existing players stay connected. During an incomplete or invalid
update it logs an error and retains the previous identity. It refuses new TLS
handshakes once that identity expires. Invalid initial material fails startup,
including startup of the plaintext listener.

## Require encrypted connections

Set `TELNET_ENABLED=false` alongside `TLS_PORT` to disable the plaintext listener.
By default both configured listeners run. The TLS listener never falls back to
plaintext when its handshake or certificate fails. It permits up to 128 concurrent
handshakes, each with a ten-second timeout.
