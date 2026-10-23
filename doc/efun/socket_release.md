# socket_release

`int socket_release(int handle, object recipient, function offer)`

Offer an owned established stream synchronously to a live recipient. Connecting
streams and listeners cannot be offered. `offer` must resolve to the recipient and
receives `(int handle, object previous_owner)` using native pointer argument checks.
It runs with efun origin, the existing caller chain and preserved player context.

Return 1 if the recipient calls `socket_acquire` during this offer, otherwise 0.
Acquisition changes ownership, callback and both inventories atomically, preserving
queued bytes and event order. Recursive offers are errors. The master must authorize
`release`; acquisition separately requires authorization for `acquire`.

Exceptions propagate. A caught exception retains prior writes, including a completed
acquisition; an uncaught error aborting the transaction discards the handover.
No separate callback savepoint is created.

### See also

[TCP sockets and events](../tcp-sockets.md), [`valid_socket`](../apply/master/valid_socket.md), [`socket_release`](socket_release.md), [`socket_acquire`](socket_acquire.md)
