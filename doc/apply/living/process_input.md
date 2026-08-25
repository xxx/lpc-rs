# process_input

`mixed process_input(string line)`

The driver applies `process_input` to a living before dispatching a command
line, from the connection and from `command()`. A string result replaces the
line; 0 (or no such function) dispatches the line as typed; any other result
consumes the line — nothing else runs.

### See also

`command`, `add_action`, `add_rule`, `notify_fail`, `command_not_found`
