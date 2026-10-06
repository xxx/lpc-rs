# socket_info

`mapping socket_info(int handle)`

Return an authorized transaction snapshot of a socket, or nil for a positive stale
handle. Invalid nonpositive handles are errors. The master must authorize `info`,
even for one's own socket or a stale handle; foreign inspection grants no mutation
rights. Ownership changes made during authorization are rechecked.

Keys are `handle`, `owner`, `kind`, `state`, `local`, `remote`, `output_pending` and
`read_eof`. Kinds are `stream` and `listener`. States are `connecting`, `binding`,
`open`, `listening`, `closing` and `closed`. Endpoints are `({ numeric_address, port })`
or nil until known. Output pending counts admitted bytes without published completion.
This includes the current transaction's writes. The result is fresh metadata, not
an OS probe or a mutable registration.

### See also

[TCP sockets and events](../tcp-sockets.md), [`valid_socket`](../apply/master/valid_socket.md)
