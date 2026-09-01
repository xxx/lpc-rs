# rm

`int rm(string path)`

Remove the file at `path`. Returns 1. A `path` that does not exist, or that
is a directory, is a runtime error naming its in-game path.

Every call is first put to the master's `valid_write(path, "rm", caller,
program)`. A refusal, or a master that does not define the apply, is the
runtime error `rm: permission denied`. All are caught by `catch()`.

The file goes when the task commits: a task that errors out removes nothing,
and a task the driver re-runs removes it once. A failure at that point goes
to the debug log.

### See also

`write_file`, `read_file`, `get_dir`, `valid_write`
