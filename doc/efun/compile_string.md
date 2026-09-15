# compile_string

`object compile_string(string name, string source)`

Compile a complete LPC program from `source`, initialize it, and return its
object. No source file is read or written for the program itself. The source
supports the normal preprocessor, includes, inheritance, and simulated efuns.

`name` is an in-game object path; relative names resolve against the executing
object's directory, and a trailing `.c` is optional. It determines the object's
identity, diagnostic filename, and relative include and inheritance paths.
The named directory need not exist. Clone names and paths outside the lib are
rejected. An already-loaded name raises an error; destruct the old object
explicitly before reusing its name. A source file already on disk is unchanged.

The master must allow both `valid_write` and `valid_load`, in that order, for
the canonical source name with `.c` appended. Both receive `func` equal to
`"compile_string"`, the calling object, and its defining program. Write
permission authorizes supplying code under that identity even though no file
is written. Missing or refusing hooks raise an error. Includes and inherits
retain their usual permission checks; the virtual-object `compile_object`
hook is not used.

Compilation and initialization join the caller's transaction. The new object
is immediately visible to `find_object`, `load_object`, and calls on its object
reference. Its initializer inherits the player context and caller chain of
the compile request. Includes and inherited sources see the transaction's
pending file changes, just as ordinary object compilation does.

An aborted attempt discards the object and its pending effects; a conflict
retries the operation. Compile errors, permission errors, and initializer
errors propagate through `catch`. A failed initializer leaves no object under
the name. Catching an error follows normal LPC semantics and does not create
a separate rollback scope. Compile warnings use the usual warning handler.

The object remains loaded until explicitly destructed. Compilation does not
execute a particular function beyond the usual initializer; the caller
chooses what to call and when to clean up.

```c
object scratch = compile_string("/wizard/scratch",
    "mixed run() { return 6 * 7; }");
mixed result;
string error = catch(result = scratch->run());
destruct(scratch);
if (error) throw(error);
write(sprintf("Result: %O\n", result));
```

### See also

`load_object`, `destruct`, `valid_write`, `valid_load`, `valid_inherit`, `valid_read`
