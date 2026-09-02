# compile_object

`mixed compile_object(string path, string func, object caller, string program)`

The driver applies `compile_object` in the master when a load names a path
that has no source file: `find_object`, `move_object`, `tell_object`,
`clone_object`, a string receiver of `->`, a `&->f()` pointer whose
receiver argument is a path, and any other efun that takes an object by
path. A string return names the blueprint whose
program the driver runs under `path`; the result is a *virtual object*: its
`file_name` is `path`, it is not a clone, and it holds its own globals.
A path with no source file is put here *before* `valid_load`: when the
answer names a blueprint that is not yet resident, `valid_load` hears the
blueprint's source, not this path, and a resident blueprint asks nothing.
When the answer declines, or the apply is undefined, the load
proceeds as an ordinary compile of the missing file: `valid_load` is asked
for `<path>.c` and, if it allows, the compile fails to read it. A path
ending in `#<n>` is never put here.

- `path` is the object name requested, as `file_name` will answer it:
  leading `/`, no `.c` (`find_object("/inst/17/d/room1")` and
  `"/inst/17/d/room1.c"->f()` both ask about `"/inst/17/d/room1"`).
- `func`, `caller` and `program` are what `valid_load` receives for the same
  door: the efun or `"call_other"`, the object whose code asked, and the
  file that defines that code.

The answer is an absolute object name (a bare `"room1"` is `/room1`). It is
a name, not a permission: the blueprint is loaded for the original
requester, so a blueprint that is not yet resident is put to `valid_load`
with `path` the blueprint's source and the same `func`, `caller` and
`program` this apply received. A `compile_object` that strips a prefix and
returns the rest therefore cannot give code an object it could not load
directly. A resident blueprint is used without asking, as `clone_object`
uses a resident prototype. The answer is never itself put back to
`compile_object`, so an answer naming no file is a plain failure. An empty
string is a path too, and is refused as one. A blueprint with `#pragma
no_clone` is refused. An object whose `create()` throws is not left
resident; the error is the caller's.

The apply runs inside the loading task: `this_object` is the master, and an
error thrown here is the caller's error. It runs in the loading attempt's
transaction, so a write it makes (registering the object with an instance
daemon) rolls back with a rejected attempt and is made again on the re-run.
Because this apply runs before the blueprint's `valid_load`, a registration
made here survives a denial the caller catches; check the caller's
permission first, or register lazily. Two tasks materializing one path get
one object.

Relative object paths resolve against the executing object's own
directory, so a room written with `find_object("room2")` reaches
`/inst/17/d/room2` from `/inst/17/d/room1` and `/d/room2` from
`/d/room1`. The `program` seen by `valid_read`, `valid_write` and
`valid_load` for code running in a virtual object is the blueprint's file:
authority follows code, not name. Teardown is the lib's: destruct the
instance's objects, and decline their paths afterwards.

### Examples

```c
// /inst/<n>/<rest> is the rest, for an instance the daemon knows.
mixed compile_object(string path, string func, object caller, string program) {
    int n; string rest;
    if (sscanf(path, "/inst/%d/%s", n, rest) != 2) return 0;
    if (!INSTANCE_D->query_instance(n)) return 0;
    return "/" + rest;
}
```

### See also

`valid_load`, `find_object`, `clone_object`, `file_name`
