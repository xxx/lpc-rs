# valid_store_variable

`int valid_store_variable(object caller, object target, string name, string declaring_program, mixed value, string program)`

The driver applies this in the master before `caller` replaces another object's
named global with [`store_variable`](../../efun/store_variable.md). A truthy
result allows the request; zero, a missing master, or a missing apply refuses
it with `store_variable: permission denied`. Self-writes do not call this apply.
Public and private declarations require the same authorization for foreign
writes. `valid_variable_info` does not authorize this operation.

`caller` is the requesting object, and `target` is a live destination object.
`name`, `declaring_program`, and `value` are the requested arguments. The
selector is nil when omitted. Its string spelling has not been normalized or
resolved: lookup happens only after permission is granted, so denial reveals
neither whether a declaration exists nor whether its name is ambiguous.
Policies that authorize particular declarations can require an explicit
canonical selector, including `.c`, instead of permitting unqualified names.

`program` is the canonical source path of the code requesting the write,
including `.c`. Inherited code names its defining source; an efun pointer names
the source that created it. It is the configured nil value when no defining
source is available, as for an efun pointer fired directly as a task entry.
A simul-efun wrapper is the `caller` when it invokes the setter, even if it
passes its own caller as `target`.

The apply joins the caller's transaction: `this_object()` is the master,
`previous_object()` is the caller, and `this_player()` is preserved. Conflict
retries may run it again; abort discards both the policy's changes and the
write. Errors propagate to the caller. If the caller catches an error or
refusal, policy side effects remain pending in that transaction.

After authorization, the driver rechecks target liveness, resolves exactly one
declaration in its current program image, and checks `value` against the retained
declared type. Approval does not bypass those checks. Destroying the target
therefore prevents the write. Reference values are shared with the caller;
mutating a proposed array, mapping, or class affects the value subsequently
checked and stored, and may remain visible even if the write fails.

```c
int valid_store_variable(object caller, object target, string name,
                         string declaring_program, mixed value, string program) {
    return 0;
}
```

## See also

`store_variable`, `variable_info`, `valid_variable_info`
