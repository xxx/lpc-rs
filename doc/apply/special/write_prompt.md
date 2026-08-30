# write_prompt

`string write_prompt()`

The driver applies `write_prompt` in the object bound to a connection after
each command line it runs, and after each `input_to` callback. Whatever it
returns is sent as the prompt, followed by the mark the client negotiated
(telnet EOR, else GA); it may also `write` its prompt and return nothing, and
the mark still follows. A body that does not define `write_prompt` gets no
prompt and no mark.

When the command or callback left another `input_to` pending, `write_prompt`
is not applied: the callback wrote its own prompt (`"Password: "`), and only
the mark is sent after it.

`this_player()` is the body.

### Examples

```c
string write_prompt() {
    return sprintf("%d/%d> ", hp, max_hp);
}
```

### See also

`input_to`, `catch_tell`
