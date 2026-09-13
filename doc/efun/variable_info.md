# variable_info

`mapping *variable_info(object ob)`

Return every global declaration in `ob`, including inherited, hidden, private,
protected, and static variables. Entries follow storage order: ancestor blocks
first, then the object's own declarations, each block in declaration order.
An ancestor shared through multiple parents appears once. Different declarations
with the same name have separate entries.

Each entry has these fields:

| Key | Type | Meaning |
| --- | --- | --- |
| `name` | string | Declared variable name |
| `program` | string | Declaring program's source path, including `.c` |
| `type` | string | Declared LPC type, such as `int` or `mixed *` |
| `flags` | string | Visibility, followed by `static` and `nomask` when present |
| `value` | mixed | Value read through the caller's transaction |

Pending writes in the same transaction are visible. The returned array and
entry mappings are new containers: replacing an entry's `value` does not assign
the original global. Values retain normal LPC sharing, however. Mutating an
array or mapping obtained through `value` changes that value in the object too;
objects and function pointers retain their normal behavior.

Self-inspection is allowed. Inspecting another object requires the master's
[`valid_variable_info(caller, target, program)`](../apply/master/valid_variable_info.md)
to return a truthy value. Refusal or a missing master/apply raises
`variable_info: permission denied`. Errors from the apply propagate.

Zero, a destructed object, or an object destructed during authorization returns
an empty array. Other argument types raise an error. This efun does not load
objects or print output.

```c
dump(variable_info(this_object()));
foreach (mapping variable : variable_info(ob)) {
    write(sprintf("%s: %s %s = %Q\n", variable["program"],
        variable["type"], variable["name"], variable["value"]));
}
```

## See also

`inherit_list`, `deep_inherit_list`, `function_info`, `dump`
