# get_char

`int get_char(string | function f, int no_echo = 0)`

Register a callback for the next character entered by `this_player()`. This
returns immediately. The callback receives a string containing one Unicode
scalar, including control characters. Input bypasses the command hooks.

`f` is a function pointer or the name of a function in the calling object,
including private and static functions. A missing name or an unbound receiver
is an error. Arguments follow ordinary function-pointer binding: bound
arguments precede the character. The callback runs in its receiver, with
`previous_object()` set to the pointer's owner, `this_player()` set to the
object attached when input arrives, and `origin() == "driver"`.

Any nonzero `no_echo` asks the client to hide input; zero enables local echo.
The driver requests character transmission using Telnet suppress-go-ahead
(SGA). Delivery still depends on the client sending its input; a client that
keeps buffering lines cannot be made to send each keystroke by this function.

Each registration consumes one character. A callback can call `get_char`
again to continue character input, or [`input_to`](input_to.md) to read a line.
Rearming with the same mode and echo setting preserves both without toggling
them between callbacks. Without a replacement, line mode and local echo are
restored after the callback. Unread input is preserved and interpreted in the
new mode, even when several characters arrived together.

Registrations replace any pending `get_char` or `input_to` handler; the last
registration wins. They take effect in order with other socket operations
after the transaction commits. An aborted attempt registers nothing; conflict
retries publish only the successful attempt. A caught error preserves a
registration when its transaction commits.

UTF-8 sequences may span network reads. Malformed subsequences become U+FFFD;
an incomplete character waits for more bytes. Combining characters are
separate scalars. Backspace, Delete, Tab, NUL, and Escape arrive as data without
line editing. An arrow-key sequence such as Escape, `[`, `A` needs three
callbacks. CR and LF remain distinct, except that Telnet CR LF and CR NUL pairs
deliver one CR. Their padding is consumed even if the callback selects line
mode. Telnet protocol commands are not character input.

The normal [`write_prompt`](../apply/special/write_prompt.md) cycle follows
the callback. Another pending input handler suppresses prompt text.
If the callback cannot be prepared, for example because its owner was destroyed
or its receiver cannot be loaded, the error goes to the master's `error_handler`
and the command giver sees `Canceled.`

Returns `1` when the command giver has a connection and registration was
recorded, or `0` when it is not interactive. This does not guarantee that the
connection remains open until delivery.

### Examples

```c
void ask()
{
    write("Continue? [y/n] ");
    get_char(answer);
}

void answer(string key)
{
    if (key == "y" || key == "Y") {
        write("Continuing.\n");
    } else if (key == "n" || key == "N") {
        write("Canceled.\n");
    } else {
        get_char(answer);
    }
}
```
