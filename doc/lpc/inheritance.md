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
override and selects the last parent defining that name. Each direct parent
also supplies an automatic name: its source filename without the directory or
trailing `.c`.

```c
inherit "/std/room";

void reset() {
    room::reset();
}
```

Relative paths, macros, and an explicit `.c` suffix produce the same name for
the same source. The driver's automatic inherit also supplies a basename.
Only direct parents supply names; use a direct parent's name to reach a function
it inherited from a more distant ancestor.

When direct parents share a basename, lookup searches them in reverse declaration
order and stops at the first one defining the requested function, matching `::`.
Parents without that function are skipped. An inaccessible private definition
stops lookup without trying another parent.

An explicit alias selects one direct parent and takes precedence over any
automatic name, regardless of inherit order:

```c
inherit "/std/room" standard_room;
inherit "/custom/room" room;
```

Here `standard_room::reset()` selects `/std/room`, and `room::reset()` selects
`/custom/room`. An explicit alias never falls back to a matching basename if
its parent lacks the function. The parent's basename remains available unless
an explicit alias takes that name.

Explicit aliases must be unique, and `efun` is reserved for driver functions.
Inheriting a file named `efun.c` does not change `efun::`; use an explicit alias
to qualify its functions. Use an explicit alias when the basename is not an LPC
identifier. String-based lookup through `call_inherited()` or `function_exists()`
can still use such a basename, for example `"base.h::get"` for `base.h.c`.
Path suffixes such as `"std/room::reset"` are not inheritance namespaces.

Automatic names and explicit aliases belong to the source containing the inherit
statement. Inherited code and its closures retain that source's lookup rules.

A private import remains usable through plain calls, qualified calls, and local
function pointers in the source containing the inherit statement. That source
can override imported functions. Once inherited again, hidden functions keep
their preceding dispatch targets; a descendant's same-named function cannot
redirect their internal calls.

[`call_inherited()`](../efun/call_inherited.md) and qualified
[`function_exists()`](../efun/function_exists.md) use the calling source's parents
and visibility, including when that source runs in a descendant object. A
private declaration selected from a parent stops lookup without trying another
parent.

External calls, external pointer creation, and external function introspection
respect the resulting visibility. An already-created local pointer remains
callable when passed to another object, as for ordinary private functions.

Inherited programs share executable code. Their globals belong to the receiving
object, and common ancestors in a diamond share global cells. Restrictions are
stored with each program's bindings; importing a program privately does not
change another object's public import of that program.
