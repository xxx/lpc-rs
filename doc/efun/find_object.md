# find_object

`object find_object(string path)`

Find the object in the object space at `path`, and return it. `path` is an 
in-game path, rooted at `LIB_DIR`. It can also include a trailing `#` and a
number, which will be used to find a specific instance of an object. If no
object is found, `0` is returned.

An object that is not resident is loaded, which asks the master's
`valid_load` with `func` `"find_object"`; a refused or failed load returns
`0` — a `valid_load` that throws is a failed load — as does a path that names
no file. A path with no source file is put to the master's `compile_object`,
which may name a blueprint to run under that path.
