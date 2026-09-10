# rm

`int rm(string path)`

Remove the file at `path`. Returns 1 when removal is scheduled, or 0 without
an error when the path does not exist, including a missing parent directory.
A directory or another filesystem failure raises a runtime error naming
the in-game path. Symlinks are removed as links, even if their targets are
missing.

Every call is first put to the master's `valid_write(path, "rm", caller,
program)`. A refusal, or a master that does not define the apply, is the
runtime error `rm: permission denied`. All are caught by `catch()`.

The file goes when the task commits: a task that errors out removes nothing,
and a task the driver re-runs removes it once. A read of the path later in
the same task already finds it missing (`file_size` answers -1). A failure
at that point goes to the debug log.

### See also

`write_file`, `read_file`, `get_dir`, `valid_write`
