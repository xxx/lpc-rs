# valid_inherit

`int valid_inherit(string path, string from)`

The driver applies `valid_inherit` in the master before a compile reads a
parent program. A non-zero return allows it; zero refuses it, and the compile
fails with `inherit "<path>": permission denied` at the `inherit` line. A
path that leads out of the lib fails before this apply.

- `path` is the parent's source file, canonical, `.c` appended
  (`inherit "/std/object";` asks about `"/std/object.c"`).
- `from` is the source file of the program being compiled — the program
  that gains the parent's code, even when the `inherit` line was brought in
  by a header. A parent that itself inherits is asked for separately, with
  the parent as `from`.

Inheriting is more than reading: an inherited function runs under the
parent's file (`program` in `valid_read`, `valid_write`, `valid_load`), so
whoever may inherit a program may act with its authority. That is why this
is its own apply and not a case of `valid_read`, and why it is refused when
the master does not define it. The configured auto-inherit file is not
asked about.

Only compiles that LPC triggers ask: boot (the simul_efun object and the
master) compiles without a master and without this apply.

### Examples

```c
// /std is for everyone; otherwise only your own code.
int valid_inherit(string path, string from) {
    if (path[0..4] == "/std/") return 1;
    return creator(path) == creator(from);
}
```

### See also

`valid_load`, `valid_read`
