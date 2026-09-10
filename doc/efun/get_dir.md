# get_dir

`string *get_dir(string path)`

The names of the entries in the directory at `path`, sorted, as plain names
(no directory prefix, no type markers). Both files and directories are
included; `({})` means an empty directory or no matching entries.

The final component of `path` can be a wildcard pattern:

- `*` matches zero or more characters.
- `?` matches exactly one Unicode scalar value.
- `\` makes the next character literal; write `\\` in an LPC string to
  pass one backslash to the pattern.

Matching is case-sensitive and covers the entire entry name, including
leading dots. `.` and `..` are never returned. Earlier directory components
are literal: patterns do not traverse directories, and `**` has the same
meaning as `*`. Bracket classes and brace expansion are not supported.
A filename containing wildcard characters does not override the pattern;
use backslash escapes to match those characters literally.

A path without wildcard syntax still lists the directory itself, with or
without a trailing slash. An ordinary file path, a missing or unreadable
directory, or a pattern ending in an unpaired backslash raises a runtime
error naming the in-game path.

Every call is first put to the master's `valid_read(path, "get_dir", caller,
program)`, once, with the full normalized in-game path including the
pattern. Relative paths resolve against the caller's directory. A refusal,
or a master that does not define the apply, is the runtime error
`get_dir: permission denied`. Errors can be caught by `catch()`.

### Examples

```c
string *names = get_dir("/players");
string *saves = get_dir("/players/*.o");
string *rooms = get_dir("room?.c");
string *literal_star = get_dir("/players/\\*.o");
```

### See also

`read_file`, `write_file`, `rm`, `valid_read`
