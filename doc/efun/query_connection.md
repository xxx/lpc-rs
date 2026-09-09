# query_connection

`mapping query_connection(object ob = this_player())`

What the driver knows about `ob`'s connection, as a mapping with a fixed key
set. Every key is always present; an unreported value is 0 except for `mtts`
and `colour_depth`, which use -1 to distinguish unknown from explicit no-color.

| key | value |
|---|---|
| `"ip"` | the client's address, as text |
| `"port"` | the client's port |
| `"cols"`, `"rows"` | the window size the client reported (NAWS); 0 until it does |
| `"charset"` | the charset negotiated (CHARSET); 0 until one is |
| `"gmcp"`, `"mxp"`, `"eor"` | 1 while that extension is on |
| `"idle"` | seconds since the client's last line of input; from connecting, before one |
| `"overflowed"` | 1 while the client is not taking its output and text to it is being dropped; only with a `MAX_PENDING_OUTPUT` bound |
| `"client_name"` | first TTYPE name, normalized to uppercase; 0 until reported |
| `"terminal_type"` | second TTYPE name, normalized to uppercase; 0 until reported |
| `"mtts"` | MTTS capability bitmask; -1 until a valid mask is reported |
| `"colour_depth"` | detected depth: 0 plain, 3 eight colors, 4 sixteen colors, 8 indexed 256 colors, 24 truecolor; -1 when unknown |

Returns 0 when `ob` has no connection. Output to the client is always UTF-8,
whatever `"charset"` says.

TTYPE negotiation starts on connection and does not delay login. An explicit
MTTS report overrides terminal-name hints: bit 256 selects truecolor, bit 8
selects indexed color, bit 1 alone selects eight ANSI colors, and no color bits
select plain text. `-TRUECOLOR` and `-256COLOR` suffixes are recognized;
bare XTERM/SCREEN/TMUX select sixteen colors; ANSI/VT100/LINUX select eight;
DUMB selects plain. Unknown names do not imply support. The eight-color choice
for generic ANSI reports is conservative; the mudlib can explicitly select
another depth in `terminal_colour`.

Repeated terminal names, valid MTTS, or eight replies stop the exchange. Only
negotiated, solicited IS replies with 1–128 printable ASCII bytes are accepted.
Disabling TTYPE clears these fields. Detection describes the client's report,
not a persistent player preference.

### See also

`query_ip_number`, `interactive`, `send_gmcp`, `send_mxp`, `window_size`, `terminal_colour`
