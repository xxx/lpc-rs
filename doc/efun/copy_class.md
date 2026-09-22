# copy_class

`mixed copy_class(mixed value)`

Returns a fresh class instance of the same nominal type as `value`. The argument
must be a class instance; other values, including zero, raise a runtime error.
The caller does not need a visible definition of the class.

The copy contains every materialized field view and all retained field values,
including fields used only by older compiled code. Copying does not acquire a
new view. Compatible fields shared between views still share storage within
the copy.

```c
class position { int x; int *history; }

class position original = new(class position, x: 4, history: ({ 1 }));
class position duplicate = copy_class(original);
duplicate.x = 9;           // original.x remains 4
duplicate.history[0] = 2;  // original.history[0] is now 2
```

The copy has a different instance identity and can be used as a separate mapping
key. Replacing its fields does not change the original. Arrays, mappings,
objects, functions, and nested class instances remain shared references. A field
pointing to the original instance still points to the original after copying.
Cycles are therefore supported without recursive copying.

Use [`deep_copy`](deep_copy.md) to copy nested mutable containers too, preserving
shared references and cycles within the copied graph.

Copying reads the source's values in the current transaction, including pending
writes. The allocation commits or aborts with that transaction; a concurrent
source change can cause a conflict and retry.

See also: [deep_copy](deep_copy.md), [classp](classp.md), [classes](../lpc/classes.md).
