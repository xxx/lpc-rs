# socket_acquire

`void socket_acquire(int handle, function callback)`

Accept an active synchronous `socket_release` offer and install the event callback.
Only the named recipient, executing during that offer, can acquire its handle.
A third object, deferred invocation, wrong handle or repeated acquisition is an error.
The callback receives `(int handle, string event, mixed data)`.
The offer belongs to its active LPC call stack; a separate native apply task does
not inherit it.

The master must authorize `acquire`; ownership, liveness and offer state are checked
again afterward. Successful acquisition updates registration and owner inventories
in the same transaction. Subsequent input uses the new callback. An uncaught error
aborting that transaction discards the transfer; a caught later error preserves it.

### See also

[TCP sockets and events](../tcp-sockets.md), [`valid_socket`](../apply/master/valid_socket.md), [`socket_release`](socket_release.md), [`socket_acquire`](socket_acquire.md)
