# function_exists

`mixed function_exists(string name, object | string ob = this_object())`

Return the file that defines the function `name` in `ob`, in the form
`file_name` uses (`"/std/object"`), or `0` when `ob` has no such function.
The file is the defining program's, so an inherited function names the
parent that defines it, not `ob`'s own file. A string `ob` is loaded first,
as `call_other` would load it.

Only `ob`'s own functions count: an efun or a simul-efun is not one. When
`ob` is not the calling object, its `private` and `protected` functions are
hidden and answer `0`.

### Examples

```c
if (function_exists("query_weight", ob)) {
    weight += ob->query_weight();
}
```

### See also

`call_other`, `functionp`, `file_name`
