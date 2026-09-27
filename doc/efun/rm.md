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

The existence and type checks include earlier file effects in the same task,
including effects recorded by `valid_write`. A newly written or saved file can
be removed before it reaches disk. Removing the same entry twice returns 1,
then 0; recreating it makes it removable again. Pending directory creations
are still rejected as directories. Renames move the entry to the destination,
using its type at the time of the rename; renaming a path to itself leaves it
unchanged. These checks inspect metadata without reading file contents or
requiring `valid_read`.

Pending effects are matched by exact normalized path. Moving an ancestor
directory does not update its descendants' paths in this view, and changes
through another symlink or hard-link name are not propagated between aliases.
This is not a filesystem snapshot: external changes and commit-time IO failures
can still prevent a scheduled operation from succeeding.

The file goes when the task commits: a task that errors out removes nothing,
and a task the driver re-runs removes it once. A read of the path later in
the same task already finds it missing (`file_size` answers -1). A failure
at commit goes to the debug log.

### See also

`write_file`, `read_file`, `get_dir`, `valid_write`
