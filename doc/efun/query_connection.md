# query_connection

`mapping query_connection(object ob = this_player())`

What the driver knows about `ob`'s connection, as a mapping with a fixed key
set. Every key is always present; a value the client has not reported is 0.

| key | value |
|---|---|
| `"ip"` | the client's address, as text |
| `"port"` | the client's port |
| `"cols"`, `"rows"` | the window size the client reported (NAWS); 0 until it does |
| `"charset"` | the charset negotiated (CHARSET); 0 until one is |
| `"gmcp"`, `"mxp"`, `"eor"` | 1 while that extension is on |

Returns 0 when `ob` has no connection. Output to the client is always UTF-8,
whatever `"charset"` says.

### See also

`query_ip_number`, `interactive`, `send_gmcp`, `send_mxp`, `window_size`
