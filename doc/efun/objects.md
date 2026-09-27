# objects

`object *objects()`

Returns a new array of every live loaded object, in unspecified order. The list
includes prototypes, clones, system objects and objects without an environment
or connection. It includes surviving clones even if their prototype was
removed or replaced. It does not load objects or truncate the result.

Membership comes from the calling transaction's snapshot, including its own
creations and excluding its own destructions. An object is listed during its
initializer; a failed initializer removes its pending placement. Concurrent
membership changes can cause the transaction to retry. Changes to unrelated
globals do not invalidate the list.

Each call returns independent array storage. Editing the array does not change
the loaded objects. Later creations do not appear in an earlier result, and
references to objects later destructed behave like other destructed-object
references.

Enumeration takes time and result storage proportional to the number of loaded
objects, plus any placements made by the current transaction. Use
[`object_clones`](object_clones.md) when only one compiled program's clones are
needed, or [`users`](users.md) for connected objects.

```c
object *all = objects();
object *living_objects = filter(all, living);

foreach (object ob in all)
    write(file_name(ob) + "\n");
```

### See also

[`object_clones`](object_clones.md), [`find_object`](find_object.md),
[`users`](users.md), [`all_inventory`](all_inventory.md), [`filter`](filter.md)
