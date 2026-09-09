# object_clones

`object *object_clones(object ob)`

Returns the live clones sharing `ob`'s compiled program, most recently
inserted first. `ob` may be a prototype or a clone; the prototype itself
is excluded. A clone includes itself in the result.

Returns an empty array for `0`, a destructed object, or a program with no
clones. The argument is required and must be an object, not a path string.

Clones created or destructed during the current task are reflected immediately.
Reloading a prototype starts a separate group; an older surviving clone
still returns the clones of its own program version.

```c
object sword = clone_object("/obj/sword");
object *swords = object_clones(sword);
// Also: object_clones(find_object("/obj/sword"))
```

This follows CD's `object_clones(object)`. LDMud calls its equivalent
`clones` and offers version-selection flags; FluffOS's `children(string)`
also includes the prototype.

### See also

`clone_object`, `find_object`, `destruct`
