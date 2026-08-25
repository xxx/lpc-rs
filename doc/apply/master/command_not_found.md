# command_not_found

`mixed command_not_found(object living, string line)`

Applied in the master when a command line is handled by no rule and no
`notify_fail` message is pending. A string result is delivered to the living;
0 delivers nothing. Without this apply the driver delivers `What?`, or the
implementation hint for a living with neither `process_input` nor any rule.

### See also

`notify_fail`, `process_input`
