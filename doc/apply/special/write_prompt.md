# write_prompt

`string write_prompt()`

The driver applies `write_prompt` in the object bound to a connection after
each command line it runs, and after each `input_to` callback. Whatever it
returns is sent as the prompt, followed by the mark: telnet EOR when the
client negotiated it, else GA — unless the client asked us to suppress
go-aheads (SGA), in which case no mark is sent at all. It may also `write` its
prompt and return nothing, and the mark still follows. A body that does not
define `write_prompt` gets no prompt and no mark. A `write_prompt` that
errors gets the mark alone; the error goes to the master's `error_handler`.

When the command or callback left another `input_to` pending, `write_prompt`
is not applied and the mark alone is sent, whether or not the body defines it:
the callback wrote its own prompt (`"Password: "`).

The first prompt after `logon()` succeeds gets the same cycle, so a login
object's `"Name: "` is marked like any other prompt.

`this_player()` is the body.

### Examples

```c
string write_prompt() {
    return sprintf("%d/%d> ", hp, max_hp);
}
```

### See also

`input_to`, `catch_tell`
