# query_verb

`string query_verb([int full = 0])`

The verb of the command in progress: as registered, or — for an `AA_SHORT`
rule, or when `full` is nonzero — the first word as typed. 0 outside a command.

### See also

`add_action`, `query_command`
