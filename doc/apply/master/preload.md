# preload

`void preload(string file)`

The driver applies `preload` on the master for each string `epilog`
returned, in order, after the master is initialized and before it listens.
Each call is its own task: what one commits stays committed when a later
one fails, and a failing one commits nothing of its own. Its error goes to
`error_handler` with the master as `error["object"]`, and the next file is
preloaded regardless.

`file` is the entry exactly as `epilog` listed it; the driver does not load
it. Loading is the master's job, usually by calling through the path, and a
lib that wants a missing file to be quiet checks `file_size` first.

Without `preload`, a non-empty list is noted in the debug log and nothing
is loaded.

### Examples

```c
void preload(string file) {
    if (file_size(file + ".c") < 0) {
        return;
    }
    file->preload_link();
}
```

### See also

`epilog`, `error_handler`, `file_size`
