# modify_command

`mixed modify_command(string line, object living)`

Applied in the master to every command line before it is dispatched, whether
typed at a connection or run through `command()`. A string result replaces
the line; any other result consumes it, so nothing is dispatched and
`command()` returns 1. Without this apply the line is dispatched as given.
The living's own `process_input` then sees the modified line. In
`modify_command`, `previous_object()` is the living.

### Examples

```c
// Strip the "$" that marks a forced command as not to be substituted.
string modify_command(string line, object living) {
    while (strlen(line) && line[0] == '$') line = line[1..];
    return line;
}
```

### See also

`command`, `process_input`, `command_not_found`, `previous_object`
