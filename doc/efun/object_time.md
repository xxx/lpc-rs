# object_time

`int object_time([object ob])`

The time `ob` — the calling object by default — was created, in seconds
since the epoch as `time()` counts them: a blueprint's load, a clone's
`clone_object`, a virtual object's `compile_object`. A string `ob` is
loaded by path. A destructed or non-object argument is an error.

### Examples

```c
int age = time() - object_time();
```

### See also

`file_time`, `time`, `ctime`, `clone_object`
