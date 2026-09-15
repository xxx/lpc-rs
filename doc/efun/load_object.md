# load_object

`object load_object(string path)`

Return the existing object at `path`, or load and initialize it if needed.
An existing object is returned without running its initializer again.
Use `find_object` to check whether an object exists without loading it.

`path` is an in-game path, rooted at `LIB_DIR`; relative paths resolve
against the executing object's directory. A trailing `.c` is optional.
Existing clones can be returned using a trailing `#` and number; new
clones must be created with `clone_object`.

Loading asks the master's `valid_load` with `func` `"load_object"`. A path
with no source file is put to `compile_object`, which may name a blueprint
to run under that path. A refused or failed load returns `0`, including
invalid paths, missing files, compilation errors, and errors thrown by a
master hook or initializer. An object whose initializer fails is not left
resident.

Loading joins the caller's transaction, so the object is immediately
visible to `find_object` and is committed with the caller's changes.

Source lookup and compilation see the caller's pending file changes,
including newly written or removed source files, headers, and inherited
programs. A `write_file` followed by a load can compile the new contents
before the file is written to disk. An already-loaded object is still reused;
destruct it and explicitly call `load_object` to load a changed source file.
A string-based call does not recreate an object destructed in that transaction.
This view adds the caller's pending changes to disk contents; it does not make
external filesystem edits part of the STM snapshot.

### See also

`find_object`, `clone_object`, `compile_string`, `valid_load`, `compile_object`
