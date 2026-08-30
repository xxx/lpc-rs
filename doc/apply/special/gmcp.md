# gmcp

`void gmcp(string package, string payload)`

The driver applies `gmcp` in the object bound to a connection whenever the
client sends a GMCP message. `package` is the message name (`Core.Hello`,
`Char.Login`, …); `payload` is whatever followed it, usually JSON, as text —
empty when the message was the name alone.

Messages arrive whether or not an `input_to` is pending. Messages sent before
a body is bound (during login, before `logon` returns) are dropped. A body
that does not define `gmcp` hears nothing. `this_player()` is the body.

GMCP is offered to every client; `query_connection()["gmcp"]` says whether
this one took it.

### See also

`send_gmcp`, `query_connection`
