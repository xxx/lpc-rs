# present

`object present(string id, object env = 0)`
`object present(object ob, object env = 0)`

Find an object by id, or check that a known object is nearby.

With a string, return the first object whose `id(id)` apply answers true,
searching the calling object's inventory and then its environment's
inventory, in that order; with `env`, only `env`'s inventory. A trailing
number picks a later match, counted across both places: `"sword 2"` is the
second object answering `id("sword")`. An object that defines no `id()`
never matches. Each `id()` runs as a call from the calling object, so
`previous_object()` inside it is that object. It runs on the calling
task's own call stack, so nesting is bounded by the call stack rather than
the task chain.

With an object, return `ob` when it is in the calling object's inventory or
in its environment's inventory; with `env`, when it is in `env`'s inventory.

Returns `0` when nothing matches, when `ob` is elsewhere, or when `env` has
been destructed. A first argument that is neither a string nor an object,
or an `env` that is neither an object nor `0`, is an error.

### Examples

```c
int do_open(string str) {
    object chest = present(str);          // "chest", or "chest 2"
    if (!chest) return notify_fail("Open what?\n");
    ...
}

if (present(key, this_player())) {        // the player is carrying it
    ...
}
```

### See also

`all_inventory`, `environment`, `move_object`, `living`
