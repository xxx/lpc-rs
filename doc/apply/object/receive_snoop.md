# receive_snoop

`void receive_snoop(string text, object target, string direction)`

Receive an observation from the connection of `target`. An observer must
implement an apply accepting these three arguments without reference
parameters. Return values are ignored. Interactive and noninteractive
observers both use this apply; delivery never writes to an observer's socket
automatically.

`direction` is `"input"` or `"output"`:

- Input is decoded connection text before command rewriting or input callback
  dispatch. Lines include a final LF; character events have no added terminator.
  Pending no-echo input is always excluded, even if echo negotiation is refused.
  Synthetic `command()` calls create no input observation. Actual input remains
  observable if its later command fails.
- Output is text accepted by the connection, including prompts, `write_socket`
  and driver text notices. Chunk boundaries and text are preserved without a
  prefix. Text intercepted by `catch_tell` or dropped by the pending-output
  limit is excluded; the truncation notice is included. Telnet framing, GMCP
  and MXP operations are excluded. Escape sequences in ordinary text remain.

Each callback runs in a fresh transaction with normal evaluation limits.
`this_object()` is the observer, `origin()` is `"driver"`, there is no previous
LPC object, and `this_player()` and `this_interactive()` are nil. The explicit
target argument does not confer that user's execution context. Delivery enters
the observer's own apply, including inherited implementations, without shadow
redirection.

Callbacks are serialized per observer and preserve a registration's event
order. There is no global order across connections. They cannot filter or veto
the observed user's activity. Stopping observation discards queued and
uncommitted callbacks; effects already committed may still arrive.

The queue counts pending and in-flight text against `LPC_MAX_PENDING_SNOOP`
(default 65,536 UTF-8 bytes; must be positive) and a 1,024-record bound. Empty
output is skipped. Overflow immediately stops capture and discards pending
records, logs one warning and schedules transactional retirement. Callback
errors, missing applies and incompatible applies after recompilation roll back
that callback and retire the registration with a diagnostic. The target stays
connected. Attach again explicitly to resume; this is not a durable audit log.

### Example

An interactive observer can forward text to its own connection explicitly:

```c
void receive_snoop(string text, object target, string direction) {
    write_socket("[" + direction + "] " + text);
}
```

### See also

[`snoop`](../../efun/snoop.md), [`valid_snoop`](../master/valid_snoop.md)
