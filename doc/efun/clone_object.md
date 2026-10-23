# clone_object

`object clone_object(string path, ...)`

Create a new instance of an object from the blueprint at `path`,
and return it. `path` is an in-game path, rooted at `LIB_DIR`.

Arguments after `path` are forwarded by value to the clone's
[`create`](../apply/object/create.md) hook, after all inherited and local global
initialisers have run. Arrays and mappings retain their identity, so mutations
through them are visible to the caller. For example, `clone_object("/obj/torch",
60)` calls the new torch's `create(60)`. Array spreading is also supported:
`clone_object("/obj/torch", args...)`.

The object defines its own constructor signature. Omitted parameters receive
their default values or `0`; excess arguments are ignored unless `create`
declares `...`, which collects them in `argv`. Supplied arguments must match
the declared parameter types, and reference parameters are not supported.
An object without `create` can be cloned with arguments; they are ignored.

A prototype that is not yet resident is compiled first, which asks the
master's `valid_load`; a refusal is the runtime error `clone_object:
permission denied`. A `path` with no source file is put to `compile_object`;
cloning a virtual object clones its blueprint's program under the
blueprint's name. A newly loaded prototype is initialised without constructor
arguments; the supplied arguments belong only to the clone. Cloning a resident
prototype asks nothing and does not rerun its constructor.

This function is the primary way of constructing new object instances in LPC.

A blueprint with [`#pragma no_clone`](../lpc/pragmas.md#no_clone) cannot be
cloned; the call raises a runtime error.

A `create()` that clones again nests one driver task per level; exceeding
`MAX_TASK_CHAIN` raises a runtime error. A clone whose `create()` throws is not
left resident. Construction joins the caller's transaction: an uncaught error
rolls back its state changes, and a conflict retries the enclosing task.

Use `object_clones` to list the live clones of the same compiled program.
