# warning_handler

`void warning_handler(mapping warning)`

The driver applies `warning_handler` in the master once per warning a
successful compile raised, in the order the compiler raised them (an inherited
file's warnings included), before the compiled object is inserted. A compile
that fails carries its warnings inside the error instead, so they arrive with
it — at `error_handler`, or in the `catch`.

The warning mapping has the following keys:

* `warning["message"]` (string) - The warning message.
* `warning["location"]` (string) - The in-game `file:line:column` the warning
  points at.
* `warning["file"]` (string) - The in-game path of the file being compiled.
  A path, not an object: the warning is raised before the object exists.
* `warning["diagnostic"]` (string) - The rendered diagnostic, source excerpt
  included.

The apply runs inside the loading task: `this_player` is whatever it was at
the `load_object`, `clone_object`, or call that triggered the compile, so a
coder updating their own file is `this_player` here. An error thrown in the
apply is the loader's error, and the object is not loaded.

When the master does not define `warning_handler`, or no master exists yet
(the master's own compile), warnings go to the debug log.

### Examples

```c
void warning_handler(mapping warning) {
    // Tell the coder who triggered the load; log the rest.
    if (this_player()) {
        tell_object(this_player(), warning["diagnostic"]);
    }
}
```

### See also

`error_handler`
