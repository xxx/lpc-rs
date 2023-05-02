# create

`mixed create()`

The driver calls `create` on all objects when they are initialized,
which happens when cloned, loaded from a save file, or called for the 
first time via `call_other`. An object will only be initialized once.

This happens _after_ global variables have been initialized, so they are all
available and can be used at whim.

Your mudlib may define more specific creator functions for your objects, which
you should override instead when they exist. (Typically `create` will be defined
as `nomask` in these cases.)

Defining `create` is optional, and it will simply not be called if not defined.
Note that a `create` function might be inherited from a parent object, and so
may be called even if not explicitly defined in the current object.

## See also

`clone_object`, `restore_object`, `call_other`