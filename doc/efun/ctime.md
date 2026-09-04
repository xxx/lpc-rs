# ctime

`string ctime(int t = time())`

Return `t`, a time in seconds since the Unix epoch, as text in the driver's
local time zone, in the fixed 24-character form:

```
Thu Sep  3 18:39:00 2026
```

The day of the month is space-padded to two characters. With no argument,
the current time is formatted. A `t` that no calendar can represent is an
error.

### Examples

```c
write("It is now " + ctime() + ".\n");
write("The epoch was " + ctime(0) + ".\n");
```

### See also

`time`
