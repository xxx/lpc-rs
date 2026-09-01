# valid_read

`int valid_read(string path, string func, object caller, string program)`

The driver applies `valid_read` in the master before every efun that reads
the filesystem — `read_file`, `get_dir`. A non-zero return allows the read;
zero refuses it, and the efun raises `<efun>: permission denied` in the
caller. A path that leads out of the lib fails before this apply.

- `path` is the canonical absolute in-game path being read: leading `/`, no
  `.` or `..`, a relative argument already resolved against the caller's
  directory.
- `func` is the efun's name (`"read_file"`), so one apply serves them
  all. (`efun` itself is a reserved word: `efun::name()` calls the real
  efun.)
- `caller` is the object whose code called the efun — what `this_object()`
  names there.
- `program` is the in-game path of the file that *defines* the calling code,
  extension included (`"/secure/master.c"`). An inherited function names the
  file that defines it, not the inheriting object's; a closure names the file
  it was written in; a simul_efun names the simul_efun file. When nothing
  called the efun from LPC — an efun pointer fired straight from `call_out` —
  `program` is 0.

`program` is what to build policy on: it says where the code came from, and no
object can change it by asking another object to do the reading.

The read is refused when the master does not define `valid_read`, so a master
that never defines it has no file reads at all. The apply runs inside the
caller's task: `this_player` is whatever it was, `this_object` is the master,
and an error thrown here is the caller's error.

### Examples

```c
// Code under /secure may read anything; nothing else reads /secure.
int valid_read(string path, string func, object caller, string program) {
    if (!program) return 0;
    if (program[0..7] == "/secure/") return 1;
    return path[0..7] != "/secure/";
}
```

### See also

`valid_write`, `read_file`, `get_dir`, `valid_exec`
