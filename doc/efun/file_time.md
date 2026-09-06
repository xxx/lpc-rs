# file_time

`int file_time(string path)`

The time the file at `path` was last modified, in seconds since the epoch
as `time()` counts them; `-1` when there is no such file. The master's
`valid_read` is asked first, and a refusal answers `-1` too, so the call is
a safe probe like `file_size`. A directory answers its own modification
time. `path` is resolved against the calling object's directory. A
non-string `path`, or one that leaves the lib, is an error.

### Examples

```c
if (file_time(source) > object_time(ob)) {
    // the source changed after the object was loaded
}
```

### See also

`file_size`, `object_time`, `time`, `ctime`
