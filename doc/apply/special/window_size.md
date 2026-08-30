# window_size

`void window_size(int cols, int rows)`

The driver applies `window_size` in the object bound to a connection whenever
the client reports its window size (telnet NAWS), and once more each time a
body is bound to the connection — at `logon`, and at every `exec` — if the
client has reported one. A client that never reports a size never causes a
call; `query_connection()` answers 0 for both until it does.

`this_player()` is the body.

### See also

`query_connection`, `exec`
