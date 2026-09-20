# request_object_recompile

`int request_object_recompile(object prototype)`

Queue an in-place upgrade of a loaded prototype and the live clones sharing its
current compiled program. Returns a positive request ID. Work starts after the
calling transaction commits; an aborted transaction queues nothing. Inspect the
result in a later invocation with `query_object_recompile(id)`.

The active master must allow `valid_recompile(prototype, caller, program)`, both
when requested and when preparation starts. A missing hook denies the operation.
The requester and its original command giver must remain live. The target must be
a live, initialized file prototype; clones, virtual instances, and driver-owned
master/simul-efun objects are refused. Use `request_system_reload` for those
driver-owned objects.

The driver compiles the prototype's backing source file and upgrades it and its
current clone group in one transaction. Object references, names, creation times,
connections, commands, environment and inventory remain intact. Each object's
globals are preserved independently when declaring source, name and declared type
match. Private and inherited variables are included. Retained arrays, mappings,
objects and function values keep their identity.

Each object's new global initializers run first, then matching old values are
restored. New, renamed and type-changed variables keep their initialized values,
or zero without an initializer. Removed variables disappear from the object's
layout. `create()` does not run again. Initializer effects and writes commit with
the upgrade. Errors or execution limits discard the whole preparation, leaving
the old group active. Conflicting changes retry preparation.

Old local function pointers and anonymous functions become stale after their
receiver upgrades: invoking them raises `stale function pointer after object
recompilation`. This includes pointers stored in globals, actions, `input_to`,
and pending `call_out`s. Scheduled work is retained; stale callbacks fail when
invoked. Recreate local callbacks after upgrading. Dynamic pointers, efun pointers
and name-based simul-efun pointers keep their ordinary behavior.

An inheriting program retains its compiled copy; update inheritors separately.
Clones left behind by an earlier destruct/load cycle belong to a different group
and are unchanged. New clones use the new program after publication.

This version refuses a group containing a member in an active shadow chain, and
changes that add or remove cleanup eligibility (a nonresident program defining
`clean_up`). Initializers must leave every target live and initialized, with no
active shadow chain. Requests for either kind of reload during preparation are
refused. Normal destruct/load remains available with its existing semantics.

```c
int upgrade_id;

void upgrade_swords() {
    upgrade_id = request_object_recompile(find_object("/obj/sword"));
}

void show_upgrade() {
    mapping status = query_object_recompile(upgrade_id);
    if (status) write(sprintf("%s: %d objects; %s\n",
        status["state"], status["updated"], status["error"]));
}
```

### See also

`query_object_recompile`, `valid_recompile`, `object_clones`, `request_system_reload`
