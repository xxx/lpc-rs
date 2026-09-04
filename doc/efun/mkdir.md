# mkdir

`int mkdir(string path)`

Create the directory `path`, once the master's `valid_write` allows it, and
return `1`. The path must be free and its parent must exist; either
failing, or a refusal by the master, is an error. `path` is resolved
against the calling object's directory.

The directory is created when the task commits. A task that ends in an
error creates nothing.

### See also

`rmdir`, `rm`, `get_dir`, `file_size`, `write_file`
