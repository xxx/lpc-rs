# valid_destruct

`int valid_destruct(object caller, object target, string program)`

The driver applies this in the master before `destruct(target)` removes a live
object. A truthy result allows destruction; zero refuses it. A missing master
or apply also refuses it. Refusal raises `destruct: permission denied`.
Self-destruction and calls from the master require authorization too.

`caller` is the object requesting destruction and `target` is the object it
wants to remove. `program` is the source path of the requesting code, including
`.c`. Inherited code names its defining program; an efun pointer names the
program that created it. The path is zero when no defining program is available,
as for an efun pointer fired directly as a task entry.

For an array, the apply runs once per distinct live target in array order,
before this call removes any targets. Null and destructed targets are skipped,
including targets destroyed during an earlier authorization. A refusal or
error stops authorization and prevents this call's removals.

The apply runs in the caller's transaction: `this_object()` is the master,
`previous_object()` is the caller, and `this_player()` is preserved. Errors
propagate to the caller. An uncaught error rolls back both the apply's writes
and destruction; a caught error retains the apply's writes. Retries may run the
apply again. Use it for permission checks; calling `destruct(target)` from it
re-enters the same authorization hook.

For example, a mudlib can allow self-destruction and a trusted removal daemon:

```c
int valid_destruct(object caller, object target, string program) {
    return caller == target || program == "/secure/remove.c";
}
```

## See also

[`destruct`](../../efun/destruct.md), `valid_shutdown`, `valid_variable_info`
