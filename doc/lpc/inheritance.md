# Inheritance

An `inherit` statement imports another program's functions and global variables
into the current object. Put inherits before other declarations. The path must
be a constant string expression; adjacent strings and string concatenation work.
Paths without a leading `/` are relative to the source program's directory.

```c
inherit "/std/object";
private inherit __DIR__ "configuration" config;
protected inherit "/std/counters";
```

## Visibility

An unmodified inherit is public. An explicit `public inherit` has the same
meaning. A visibility modifier restricts the imported declarations:

| Statement | Access in the inheriting source | Access from its descendants | Calls from other objects |
|---|---|---|---|
| `inherit` or `public inherit` | Public and protected declarations | Public and protected declarations | Public functions |
| `protected inherit` | Public and protected declarations | Public and protected declarations | Imported functions are hidden |
| `private inherit` | Public and protected declarations | Imported declarations are hidden | Imported functions are hidden |

A modifier never exposes a declaration that was already private, or makes a
protected declaration public. Restrictions apply to functions and global
variables. A public wrapper defined in the inheriting source can expose an
operation deliberately:

```c
private inherit "/secure/configuration";

int configuration_ready() {
    return check_configuration();
}
```

Here the wrapper is public; the imported `check_configuration()` is private.
In a simul-efun object, only public functions become simul-efuns. Private and
protected helpers neither become callable simul-efuns nor override driver efuns.
Cached simul-efun calls and pointers check public visibility again when invoked.

The compiler accepts one of `public`, `protected`, or `private` on an inherit.
Other modifiers, including `static`, `nomask`, and `varargs`, are not supported
on inherit statements. A parent's `nomask` declarations and `no_inherit` pragma
still apply.

## Calls and aliases

Plain calls use the object's overriding definition. `::name()` bypasses a local
override and selects the last parent defining that name. An explicit alias
selects one direct parent:

```c
inherit "/std/room" room;

void reset() {
    room::reset();
}
```

Aliases are local to the source declaring them. They must be unique, and `efun`
is reserved. File basenames do not create aliases automatically.

A private import remains usable through plain calls, qualified calls, and local
function pointers in the source containing the inherit statement. That source
can override imported functions. Once inherited again, hidden functions keep
their preceding dispatch targets; a descendant's same-named function cannot
redirect their internal calls.

[`call_inherited()`](../efun/call_inherited.md) and qualified
[`function_exists()`](../efun/function_exists.md) use the calling source's parents
and visibility, including when that source runs in a descendant object. A
private declaration selected from a parent stops lookup; it does not fall back
to an earlier parent.

External calls, external pointer creation, and external function introspection
respect the resulting visibility. An already-created local pointer remains
callable when passed to another object, as for ordinary private functions.

Inherited programs share executable code. Their globals belong to the receiving
object, and common ancestors in a diamond share global cells. Restrictions are
stored with each program's bindings; importing a program privately does not
change another object's public import of that program.
