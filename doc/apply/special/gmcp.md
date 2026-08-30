# gmcp

`void gmcp(string package, string payload)`

The driver applies `gmcp` in the object bound to a connection whenever the
client sends a GMCP message. `package` is the message name (`Core.Hello`,
`Char.Login`, …); `payload` is whatever followed it, usually JSON, as text —
empty when the message was the name alone.

Messages arrive whether or not an `input_to` is pending. They are dropped only
until the master's `connect()` has returned an object; from then on —
including while `logon()` is still running — the object bound to the
connection receives them, and the login object is the first such body. A login
object that defines `gmcp` must expect client-driven calls before the player
has authenticated. A body that does not define `gmcp` hears nothing.
`this_player()` is the body.

GMCP is offered to every client; `query_connection()["gmcp"]` says whether
this one took it.

### See also

`send_gmcp`, `query_connection`
