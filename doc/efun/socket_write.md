# socket_write

`int socket_write(int handle, bytes data)`

Queue the whole buffer and return 1, or return 0 without queuing any prefix when
the pending byte or write-count budget is full. Empty bytes are a successful no-op.
A buffer larger than the socket's byte budget raises an error. Strings are errors;
convert text explicitly with `to_bytes`.

Only the executing owner can write, and `valid_socket` must authorize `write`.
Streams may be connecting or open, including after peer EOF. Closing streams and
listeners are not writable. Commands publish in transaction commit order; abort
publishes nothing. The driver completes physical short writes internally.

A `drained` event means a nonempty queue became empty, not peer acknowledgement.
Retry rejected data after it, checking admission again. Close suppresses that event.

### See also

[TCP sockets and events](../tcp-sockets.md), [`valid_socket`](../apply/master/valid_socket.md)
