# valid_socket

`int valid_socket(object caller, string operation, mixed *info)`

Authorize a raw TCP efun. Return nonzero to permit; zero, nil or omission denies
with a catchable error. Apply errors propagate. There is no implicit permission for
self-inspection, empty writes or repeated closes. Automatic retirement needs no grant.

The apply runs in the requesting transaction, with the master as `this_object()`,
the executing caller in the caller chain, and the entering player context preserved.
The driver rechecks liveness, ownership, lifecycle and active offer conditions after
it returns. Do not call socket inspectors here unless recursive policy calls are
intended; use the supplied fields or your own policy records.

`info` is `({ handle, owner, host, port, recipient })`:

| Operation | Handle | Owner | Host/port | Recipient |
| --- | --- | --- | --- | --- |
| `connect` | -1 | Executing caller | Requested hostname and remote port | nil |
| `listen` | -1 | Executing caller | Requested numeric address and bind port | nil |
| `write`, `close`, `info` | Requested handle | Current owner, or nil for stale info | Known remote endpoint for a stream, local endpoint for a listener | nil |
| `release`, `acquire` | Offered handle | Previous owner | Known stream remote endpoint | Offered recipient |
| `query` | -1 | Selected owner, or nil for all owners | `""`, 0 | nil |

Unknown endpoints use `""` and 0. Nil means the configured native null. Queries may
inspect another owner only with a grant. That does not grant mutation rights.
The connect hook authorizes the requested hostname, before DNS; there is no separate
resolved-address policy hook. A numeric-only allowlist can require numeric hosts.

```c
int valid_socket(object caller, string operation, mixed *info) {
    return caller == find_object("/secure/tcp_service");
}
```

This example trusts all code executing in that service; it must enforce caller,
user, destination and delegation policy itself. The shipped ulib denies raw TCP
because its chat world needs only player Telnet connections.

### See also

[TCP sockets](../../tcp-sockets.md), [`query_sockets`](../../efun/query_sockets.md)
