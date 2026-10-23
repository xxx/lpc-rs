# socket_connect

`int socket_connect(string host, int port, function callback)`

Admit an outgoing TCP stream and return a positive VM-local handle. Hostnames are
resolved asynchronously; remote ports are 1 through 65535. A returned handle is
admission, not proof of connection. IO failures arrive as `closed` events.

The executing object owns the stream. `callback` receives `(int handle,
string event, mixed data)`; use a bare function name for an unbound local callback.
Writes may queue while connecting. Abort rolls back admission without performing IO.
The master must authorize `connect` through `valid_socket`.

### See also

[TCP sockets and events](../tcp-sockets.md), [`valid_socket`](../apply/master/valid_socket.md)
