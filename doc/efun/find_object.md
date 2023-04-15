# find_object

`object find_object(string path)`

Find the object in the object space at `path`, and return it. `path` is an 
in-game path, rooted at `LIB_DIR`. It can also include a trailing `#` and a
number, which will be used to find a specific instance of an object. If no
object is found, `0` is returned.

Note that this function can return 0 even if a file exists at the given path,
if the object has not been loaded yet. This function will not load any objects.
