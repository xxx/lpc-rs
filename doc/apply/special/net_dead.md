# net_dead

`void net_dead()`

The driver applies `net_dead` to a body whose client went away without
logging out: the socket closed, reading from or writing to it failed, or the
driver dropped it for idling past `MAX_IDLE_TIME`. By
the time it runs the body is no longer interactive — `interactive()` returns
`0`, and anything it `write`s goes to the debug log. What happens to the body
next is the mudlib's decision: keep it as a link-dead player for a while,
save and destruct it, or let a later login `exec` a new connection into it.

`this_player()` is the body. The apply is optional; an error in it goes to
`error_handler`. It is not applied when the connection ends any other way — a
`destruct` of the body, an `exec` that moves the connection elsewhere, a login
that fails, or the driver shutting down — and not to a connection that has no
body yet.

### See also

`interactive`, `exec`, `logon`, `shutdown`
