# calling_program

`string | string *calling_program([int step = 0])`

The file that defines the function `calling_function(step)` names, as an
in-game path with its extension (`/secure/master.c`) — the form the
`program` argument of `valid_read` takes. An inherited function names the
file it is written in, not the object's own file. 0 where
`calling_function` is 0; `step` and `-1` as there.

### Examples

```c
// In a log function reached by "/secure/master"->log(...) from /std/room.c:
if (calling_program()[0..4] != "/std/") return;
```

### See also

`calling_function`, `previous_object`, `file_name`
