# deep_copy

`mixed deep_copy(mixed value)`

Copies the arrays, mappings, and class instances reachable from `value`, including
mapping keys and values. Each mutable container receives a fresh identity.
Changing a copied container does not change its original.

Integers, floats, strings, and immutable bytes retain their values. Objects and
functions retain their identities: copying does not clone objects, traverse their
globals, or copy a function's captured variables or bound arguments.

```c
mapping original = ([ "items": ({ 1, 2 }) ]);
mapping duplicate = deep_copy(original);
duplicate["items"][0] = 9;  // original["items"][0] remains 1
```

## Shared references and cycles

Repeated references to one source container become references to one copied
container. Cycles point back into the copied graph. Separate calls create
separate copies.

```c
mixed *shared = ({ 1 });
mixed *original = ({ shared, shared, 0 });
original[2] = original;

mixed *duplicate = deep_copy(original);
// duplicate[0] == duplicate[1], but duplicate[0] != shared
// duplicate[2] == duplicate, but duplicate != original
```

Traversal is iterative, so nesting does not consume the Rust call stack. Work
and temporary storage grow with the reachable containers and references.

## Mapping keys

Mapping insertion order is preserved. Arrays, mappings, and classes used as keys
are copied too. These types compare by identity, so an original container key
does not find the corresponding entry in the copied mapping. Copy related keys
and mappings together when the caller needs both copies:

```c
mixed *key = ({ "tag" });
mapping original = ([ key: ({ 7 }) ]);
mixed *copies = deep_copy(({ key, original }));

// copies[1][copies[0]][0] == 7
// copies[1][key] == 0
```

String and numeric keys retain their usual value comparisons. Object and
function keys retain their identities and remain usable to index the copy.

## Classes and transactions

Class copies retain their nominal type, all materialized field views, and all
stored fields, including fields accessible only to older compiled code. Nested
values in every retained field participate in the copy. Copying acquires no new
view and requires no class definition visible to the caller. Use
[`copy_class`](copy_class.md) when nested containers should remain shared.

Copying reads the current transaction's values, including pending writes. All
new containers commit or abort with that transaction. A concurrent change to any
copied source container, including a mapping key, can cause a conflict and retry.

See also: [copy_class](copy_class.md), [classes](../lpc/classes.md).
