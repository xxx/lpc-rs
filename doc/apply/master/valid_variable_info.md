# valid_variable_info

`int valid_variable_info(object caller, object target, string program)`

The driver applies this in the master before `caller` inspects another object's
globals with `variable_info(target)`. A truthy result allows inspection of every
declaration, including private and static state. Zero refuses it. A missing
master/apply also refuses it; refusal raises `variable_info: permission denied`.
Self-inspection and null/destructed targets do not call this apply.

`program` is the source path of the code that requested inspection, including
`.c`. Inherited code names its defining program; a function pointer names the
program that created it. It is zero when no defining program is available,
as for an efun pointer fired directly as a task entry.

The apply joins the caller's transaction: `this_object()` is the master,
`previous_object()` is the caller, and `this_player()` is preserved. Errors
propagate to the caller; retries may run the apply again. If the apply destroys
the target and allows inspection, the efun returns an empty array.

Authorization grants access to the actual values: arrays and mappings are
shared, and returned function pointers may be called normally.

```c
int valid_variable_info(object caller, object target, string program) {
    return program == "/secure/inspect.c";
}
```

## See also

`variable_info`, `valid_load`
