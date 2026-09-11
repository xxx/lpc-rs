# find_object

`object find_object(string path)`

Return the existing object at `path`, or `0` if it is not loaded. This does
not compile or initialize an object, or call the master's `valid_load` or
`compile_object` hooks. Use `load_object` to load an object if needed.

`path` is an in-game path, rooted at `LIB_DIR`; relative paths resolve
against the executing object's directory. A trailing `.c` is optional.
A trailing `#` and number identifies an existing clone. Invalid paths
return `0`.

Objects loaded or cloned earlier in the same transaction can be found;
objects destructed in that transaction return `0`.

### See also

`load_object`, `clone_object`, `file_name`
