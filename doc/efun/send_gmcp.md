# send_gmcp

`void send_gmcp(object ob, string package, string payload)`

Send one GMCP message to `ob`'s connection: `package` is the message name
(`Char.Vitals`, `Room.Info`, …), `payload` its body, usually JSON, as text.
An empty `payload` sends the name alone (`Core.Ping`).

Nothing is sent, and no error raised, when `ob` has no connection or its
client did not take GMCP; `query_connection(ob)["gmcp"]` says which.

The message rides the same queue as `tell_object` output, so text written
before the call reaches the client first.

### See also

`gmcp`, `query_connection`, `send_mxp`, `tell_object`
