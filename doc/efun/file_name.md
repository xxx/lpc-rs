# file_name

`string file_name(object ob)`

Return the name of `ob`: its file's in-game path without `.c` for the
object of a file, that plus `#<n>` for a clone, and the requested path for
a virtual object (`compile_object`). `0` when `ob` is destructed.
