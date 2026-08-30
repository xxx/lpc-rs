# clone_object

`object clone_object(string path)`

Create a new instance of an object from the blueprint at `path`,
and return it. `path` is an in-game path, rooted at `LIB_DIR`.

This function is the primary way of constructing new object instances in LPC.

A `create()` that clones again nests one driver task per level; sixty-four levels
deep, the next `clone_object` is a runtime error.
