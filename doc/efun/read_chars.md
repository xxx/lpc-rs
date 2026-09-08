# read_chars

`string | int read_chars(string path, int start = 0, int length = to the end)`

`read_bytes`, measured in UTF-8 characters: return `length` characters of the
file at `path` from character `start`, once the master's `valid_read` allows
it. A negative `start` counts back from the end of the file and stops at
its beginning. A read that would run past the end is cut at the end; with
no `length`, the read runs to the end. When `start` is at or past the end,
the result is `0`, not a string. A negative `length` is an error.

A character offset is found by decoding the file from its start, so the
whole file must be UTF-8; one that is not is an error naming the file.
`read_bytes` never decodes the file, so it never raises this error. Offsets
here are the ones `str[i]` uses, so a value read back with `read_chars` can
be indexed the way it was measured.

A missing or unreadable file is an error, as is the master's refusal.

### Examples

```c
string cell = read_chars("/d/map.m", y * width + x, 1);   /* one map character, whatever its width or byte length */
string tail = read_chars("/log/driver", -4000);            /* the last 4000 characters */
```

### See also

`write_chars`, `read_bytes`, `read_file`, `to_text`, `to_bytes`
