# valid_write

`int valid_write(string path, string func, object caller, string program)`

The driver applies `valid_write` in the master before every efun that
changes the filesystem — `write_file`, `rm`. A non-zero return allows the
change; zero refuses it, and the efun raises `<efun>: permission denied` in
the caller. A path that leads out of the lib fails before this apply.

The arguments are those of `valid_read`: `path` the canonical absolute
in-game path; `func` the efun's name (`"write_file"`, `"rm"`); `caller` the
object whose code called the efun; `program` the in-game path of the file
that defines the calling code, extension included, or 0 when nothing called
the efun from LPC. Who may inherit a privileged program is `valid_inherit`'s
decision.

The change is refused when the master does not define `valid_write`, so a
master that never defines it has a read-only lib. The apply runs inside the
caller's task; an error thrown here is the caller's error. The change itself
lands when the caller's task commits.

### Examples

```c
// Only code under /secure writes, and only under /data.
int valid_write(string path, string func, object caller, string program) {
    return program && program[0..7] == "/secure/" && path[0..5] == "/data/";
}
```

### See also

`valid_read`, `write_file`, `rm`
