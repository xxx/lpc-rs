# send_mxp

`void send_mxp(object ob, string markup)`

Send MXP markup to `ob`'s connection, framed as MXP secure lines (ESC[1z).
Ordinary text is never parsed as markup — the driver locks the client's
default MXP mode to locked — so tags render only through this door.

Nothing is sent, and no error raised, when `ob` has no connection or its
client did not take MXP; `query_connection(ob)["mxp"]` says which.

### See also

`query_connection`, `send_gmcp`, `tell_object`
