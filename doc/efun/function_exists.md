# function_exists

`mixed function_exists(string name, object | string ob = this_object())`

Return the file that defines the function `name` in `ob`, in the form
`file_name` uses (`"/std/object"`), or `0` when `ob` has no such function.
The file is the defining program's, so an inherited function names the
parent that defines it, not `ob`'s own file. A string `ob` is loaded first,
as `call_other` would load it.

Only `ob`'s own functions count: an efun or a simul-efun is not one. When
`ob` is not the calling object, its `private` and `protected` functions are
hidden and answer `0`.

### Qualified names

`"::foo"` searches inherited definitions, ignoring a local override of `foo`.
`"parent::foo"` uses an explicit alias or a direct parent's source basename,
including that parent's inherited functions. An explicit alias takes precedence;
otherwise matching basenames are searched in reverse declaration order for the
first parent defining `foo`, matching `"::foo"`. Both forms return the defining
file or `0`, just like a bare name.

When inspecting the calling object (omitted `ob`, `0`, or that same object),
qualified names use the source program containing the check. Inherited code
keeps its own parents and aliases even when a child reuses an alias. Closures
and pointers to this efun keep the source scope where they were created.
When inspecting another object, names use that object's top-level program.

Private declarations inherited from a parent return `0`; functions imported
by `private inherit` remain visible to the source containing that statement,
but are hidden from its descendants. Protected functions are visible only
when inspecting the calling object. If the selected definition is inaccessible,
lookup does not try another inherit. Missing functions, unknown
aliases, and malformed qualifications return `0`. Efuns and simul-efuns are
excluded, including names such as `"efun::write"`.

Lookup uses compiled inheritance metadata without loading parent objects.
A string `ob` still loads the target object normally. Clones use their shared
compiled program's metadata.

This is an inspection operation: an unresolved direct `::foo()` call remains
a compile error even inside `if (function_exists("::foo"))`. Use
[`call_inherited`](call_inherited.md) for an optional inherited call.

### Examples

```c
if (function_exists("query_weight", ob)) {
    weight += ob->query_weight();
}
```

```c
inherit "/std/object" parent;

void foo() {}

mixed inherited_foo_file() {
    return function_exists("parent::foo");
}
```

### See also

[`function_signature`](function_signature.md), `functions`, `call_inherited`, `call_other`, `functionp`, `file_name`
