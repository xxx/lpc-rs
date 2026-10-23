# socket_listen

`int socket_listen(string address, int port, function callback)`

Admit a TCP listener on a numeric IPv4 or IPv6 address. Ports are 0 through 65535;
zero requests an OS-selected port, reported in the `ready` event's local endpoint.
The positive handle belongs to the executing object. Bind failures are asynchronous
`closed` events, not admission failures.

The callback receives `(int handle, string event, mixed data)`. Accepted children
initially belong to the listener's owner and use its callback. The `accept` event
carries the child handle. Children stay paused until that callback succeeds; it
can use `socket_release` to delegate them. Closing the listener leaves successfully
delivered children independent. The master must authorize `listen`.

### See also

[TCP sockets and events](../tcp-sockets.md), [`valid_socket`](../apply/master/valid_socket.md)
