# strftime

`string strftime(string fmt, int t = time())`

Return `t`, seconds since the Unix epoch, formatted in the driver's local
time zone by `fmt`. Ordinary characters are copied; a `%` conversion is
replaced as in C's `strftime(3)`: `%Y` `%m` `%d` `%H` `%M` `%S` `%a` `%A`
`%b` `%B` `%j` `%e` `%p` `%y` `%Z` `%z` `%c` `%x` `%X` `%F` `%T` `%s` and the
rest of the C set, plus `%%` for a percent sign. An unknown conversion is
an error, as is a `t` that no calendar can represent. `%Z` is the numeric
offset, since the driver does not name its zone.

### Examples

```c
strftime("%Y-%m-%d %H:%M")            /* "2026-09-04 21:30" */
strftime("%A, %B %e", 1000000000)     /* "Saturday, September  8" */
```

### See also

`ctime`, `localtime`, `time`
