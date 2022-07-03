# clone_object

`object clone_object(string path)`

Create a new instance of an object from the blueprint at `path`,
and return it. `path` is an in-game path, rooted at `lib_dir` in `config.toml`

This function is the primary way of constructing new object instances in LPC.
