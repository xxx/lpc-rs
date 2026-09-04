# users

`object *users()`

Return every object bound to a live connection: the players logged in,
and the login object of a connection that has not finished `logon()` yet,
since it is interactive too. An object destructed in the current task is
already gone from the answer.

### See also

`interactive`, `this_interactive`, `query_connection`
