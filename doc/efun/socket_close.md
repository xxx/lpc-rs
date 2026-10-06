# socket_close

`void socket_close(int handle)`

Mark an owned socket closing immediately within the current transaction, rejecting
new writes and ownership offers. After commit an established stream drains admitted
output with a bounded timeout; a connecting stream is canceled and its queued bytes
are discarded. Reads and new accepts stop. Repeated close on a still-visible
closing or closed record is harmless, but still requires `valid_socket` authorization.

A live owner receives one `closed` event. Its terminal record is inspectable during
that callback and retires afterward, even if the callback errors. Destructing an
owner instead retires immediately, discards output and cancels its callbacks.
Retired handles are stale; their quota remains charged until transport disposal.

### See also

[TCP sockets and events](../tcp-sockets.md), [`valid_socket`](../apply/master/valid_socket.md)
