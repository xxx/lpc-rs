# localtime

`int *localtime(int t = time())`

Return `t`, seconds since the Unix epoch, broken down in the driver's local
time zone as nine ints in LDMud's order:

| Index | Field | Range |
|---|---|---|
| 0 | `TM_SEC` seconds | 0..59 |
| 1 | `TM_MIN` minutes | 0..59 |
| 2 | `TM_HOUR` hours | 0..23 |
| 3 | `TM_MDAY` day of the month | 1..31 |
| 4 | `TM_MON` month | 0..11, January 0 |
| 5 | `TM_YEAR` year | e.g. 2026 |
| 6 | `TM_WDAY` day of the week | 0..6, Sunday 0 |
| 7 | `TM_YDAY` day of the year | 0..365 |
| 8 | `TM_ISDST` daylight time | 1 when the zone's offset at `t` exceeds its standard offset |

The `TM_*` names are not shipped by the driver; define them in a lib
header. With no argument the current time is broken down. A `t` that no
calendar can represent is an error.

### Examples

```c
int *now = localtime();
write(sprintf("%d:%02d\n", now[2], now[1]));
```

### See also

`time`, `ctime`, `strftime`
