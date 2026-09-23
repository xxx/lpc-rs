# input_to

`int input_to(string | function f, int no_echo = 0)`

Register a callback for the next line entered by `this_player()`. The input
string is supplied using ordinary function-pointer argument binding.
The callback runs in its receiver, with `previous_object()` set to the pointer's
owner and `this_player()` set to the object attached to the connection when the
line arrives. Its entry has `origin() == "driver"`.

`f` is a function pointer, or the name of a function in the calling object
(`static` included). A name the object does not define is an error.

Any nonzero `no_echo` asks the client to hide its input; zero restores ordinary
local echo. Replacing a pending handler updates that setting immediately when
the registration is delivered, including while echo negotiation is pending.
Echo is restored when a hidden-input line arrives.

Registrations are delivered in call order with other socket operations after
the owning transaction commits. An aborted attempt registers nothing; conflict
retries publish only the successful attempt's operations. A caught error does
not discard a registration if the transaction later commits.

A new registration replaces the pending handler. Each handler consumes one
line and may register another handler from its callback.

When the function's receiver cannot be loaded, the error goes to the master's
`error_handler` and the command giver sees `Canceled.`

Returns `1` if the command giver has a connection and registration was recorded,
or `0` if it is not interactive. The result does not guarantee that the
connection remains open until delivery.

### Examples

```c
void init()
{
    input_to(get_name);
}

void get_name(string str)
{
    write("Hello, " + str + "!\n");
}
```
