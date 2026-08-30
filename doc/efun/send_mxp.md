# send_mxp

`void send_mxp(object ob, string markup)`

Send MXP markup to `ob`'s connection exactly as written. Ordinary text
(`write`, `tell_object`) has `<`, `>` and `&` escaped while the client has MXP
on, so markup needs this door.

Nothing is sent, and no error raised, when `ob` has no connection or its
client did not take MXP; `query_connection(ob)["mxp"]` says which.

### See also

`query_connection`, `send_gmcp`, `tell_object`
