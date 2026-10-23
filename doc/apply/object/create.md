# create

`mixed create(...)`

The driver calls `create` when an object is initialised, including clones and
objects first loaded through `call_other`. Each instance is initialised once.
Restoring saved variables into an existing object does not call `create` again.

The hook runs after all inherited and local global initialisers. Its return
value is ignored by the driver.

The signature above represents an object-defined constructor: declare whichever
value parameters the object needs, or use `void create()` for none.
[`clone_object(path, ...)`](../../efun/clone_object.md) forwards arguments after
the path to the clone's hook. Other driver loads, including loading the
prototype before cloning it, supply no arguments. Constructors must therefore
also handle omitted parameters.

Omitted parameters receive their declared defaults or the type-aware
[default value](../../lpc/null.md): zero in zero mode; in distinct mode,
`0` for int, `0.0` for float and `nil` otherwise. Extra arguments
are ignored unless the hook declares `...`, which collects them in `argv`.
Supplied arguments are checked against the declared types at runtime; `0` is
accepted for every type only in zero mode. Distinct nil fits reference types
and mixed, but not int or float. Reference parameters are not supported. These rules
apply to driver construction; explicit LPC calls to `create` follow ordinary
function-call rules.

```c
int fuel;

void create(int initial_fuel = 60) {
    fuel = initial_fuel;
}
```

Here `clone_object("/obj/torch", 120)` creates a torch with 120 units of fuel;
loading its prototype or cloning it without an argument uses 60.

Construction runs in the calling task's transaction, so conflict retries rerun
it and uncaught errors roll back its state changes. A failed clone is removed
even if the caller catches the error; catching does not roll back other state
changes made before the error. In a clone's hook, `this_object()` is the clone,
`previous_object()` is the object that called `clone_object`, and `this_player()`
is inherited from that task. Loads without a calling object or player report
the selected null value for the corresponding context.

Your mudlib may define more specific creator functions for your objects, which
you should override instead when they exist. (Typically `create` will be defined
as `nomask` in these cases.)

Defining `create` is optional, and it will simply not be called if not defined.
Note that a `create` function might be inherited from a parent object, and so
may be called even if not explicitly defined in the current object.
An overriding hook must call `::create(...)` explicitly to run its parent's
constructor; inherited global initialisers run regardless.

## See also

[`clone_object`](../../efun/clone_object.md),
[`restore_object`](../../efun/restore_object.md),
[`call_other`](../../efun/call_other.md)
