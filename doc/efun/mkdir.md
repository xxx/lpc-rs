# mkdir

`int mkdir(string path)`

Create the directory `path`, once the master's `valid_write` allows it, and
return `1`. The path must be free and its parent must exist; either
failing, or a refusal by the master, is an error. `path` is resolved
against the calling object's directory.

The directory is created when the task commits, but the task's later file
efuns already see it: a `write_file`, `save_object`, `rename` or `mkdir`
under it succeeds, and a second `mkdir` of it is an error. A task that ends
in an error creates nothing.

### See also

`rmdir`, `rm`, `get_dir`, `file_size`, `write_file`
