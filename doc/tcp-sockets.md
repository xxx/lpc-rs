# TCP sockets

Raw TCP sockets belong to LPC objects and exchange immutable `bytes`. They perform
no text decoding, Telnet negotiation, packet framing or TLS. Use
[`socket_connect`](efun/socket_connect.md) or [`socket_listen`](efun/socket_listen.md)
with a callback accepting `(int handle, string event, mixed data)`.

```c
int connection;
void event(int handle, string name, mixed data) {
    if (name == "ready") socket_write(handle, to_bytes("hello\n", "UTF-8"));
    if (name == "read") dump(data);
    if (name == "eof") socket_close(handle);
    if (name == "closed") connection = 0;
}
void start() { connection = socket_connect("127.0.0.1", 9000, event); }
```

The master must grant each entry point through [`valid_socket`](apply/master/valid_socket.md).
Missing policy denies access. A handle identifies one VM-local resource, is positive
and is never reused, including after an aborted creation. It is not an OS descriptor
and cannot identify a resource after driver restart. The executing object owns the
socket; a simul-efun wrapper owns sockets it creates, while an inherited wrapper runs
in the inheriting object.

## Events

| Event | Data | Meaning |
| --- | --- | --- |
| `ready` | Mapping with `kind`, `local`, `remote` | Connect or bind succeeded, before reads or accepts. |
| `accept` | Integer child handle | A visible established stream initially owned by the listener's owner, using its callback. |
| `read` | Nonempty bytes | Raw bytes, with arbitrary fragmentation and concatenation, including NUL. |
| `eof` | nil | Peer stopped sending; prior reads precede this event and local writes remain possible. |
| `drained` | nil | A nonempty output queue became empty; retry rejected writes after rechecking admission. |
| `closed` | Mapping with `reason`, `operation`, `error`, `local`, `remote` | Terminal notification; reasons are `local`, `error`, `timeout`. Local close has nil operation/error. |

Endpoints are `({ numeric_address, port })` or nil until known. Accepted children
get no separate `ready`. Their reads and writes stay paused until the listener's
accept callback succeeds. That callback can synchronously offer the child to a
connection object using [`socket_release`](efun/socket_release.md) and
[`socket_acquire`](efun/socket_acquire.md). Failure or a dead registration retires
the undelivered child. Closing a listener leaves successfully delivered children
independent. EOF is delivered once, separately from terminal closure.

Callbacks run serially per handle; different sockets can progress concurrently.
Each callback attempt rereads the current owner, registration and retained event.
Success commits LPC writes and acknowledgement together; conflict retries the same
event. Uncaught callback errors roll back writes, report through `error_handler`
and discard the event. Failed accepts retire children; closed callbacks retire
metadata even on error. An unresolvable callback retires its socket.

Async entries have `origin() == "driver"`, nil player and interactive context, and
the scheduling owner in the caller chain. Native pointer type checks and named/closure
recompilation rules apply. Nil means the VM's configured native null representation.

## Ordering and limits

Creation and writes are admitted transactionally. OS setup and output begin after
commit; abort performs no IO. Per-socket commands follow source order within a
transaction and transaction commit order across tasks. Successful admission is not
successful delivery: later OS failures are callback events.

The executing owner alone can write, close or release. Whole-buffer writes return
zero on queue saturation without admitting a prefix; impossible individual sizes
raise errors. Close rejects new work immediately and drains established output with
a timeout. Connecting close cancels setup and discards output. Destruction retires
owned sockets immediately without callbacks into the destroyed owner.

The environment controls these positive limits:

| Setting | Default |
| --- | --- |
| `LPC_MAX_SOCKETS` | 256 resources per VM |
| `LPC_MAX_PENDING_SOCKET_INPUT` | 65,536 bytes per stream |
| `LPC_MAX_PENDING_SOCKET_OUTPUT` | 65,536 bytes per stream |
| `LPC_MAX_PENDING_SOCKET_WRITES` | 128 writes per stream |
| `LPC_MAX_PENDING_SOCKET_ACCEPTS` | 16 accepts per listener |
| `LPC_SOCKET_CONNECT_TIMEOUT` | 10,000 ms, including DNS |
| `LPC_SOCKET_CLOSE_TIMEOUT` | 10,000 ms |

Plain names also work; `LPC_` names take precedence. Socket count includes pending
creations, accept reservations, terminal records awaiting dispatch and hidden
retirement charges until commands, workers and descriptors are disposed. Input and
accept pressure pauses physical reading/accept activation; it does not drop bytes.
These budgets do not limit bytes retained by LPC after delivery and do not change
the task execution allowance.

Driver boot starts TCP before preloads; `run` starts it if necessary. Embedded Rust
users can call `Vm::start_sockets` after installing their master policy. Shutdown
cancels TCP callbacks and closes physical resources before the master's final
`shutdown()` hook and before the committer stops,
without promising application drain or new terminal callbacks.
