# Classes

Classes are shared, nominally typed records. They support declarations,
construction with named initializers, member reads and writes, shallow copying,
and field evolution across compiled definitions. General reflection and
persistence are not implemented yet.

```c
class position {
    int x, y;
    class position next;
}

class position current;
class position *history;

class position absent() {
    return 0;
}
```

A definition declares an ordered list of named, typed fields. A trailing
semicolon after the definition is optional. Field names must be unique within
the class. Fields cannot have type `void`, initializers, or function bodies.
Class names can be referenced before their definitions in the same compilation,
including self-referential and mutually referential fields.

Named class types work in global and local declarations, function parameters and
returns, casts, and `foreach` declarations. `class position *` is an array type.
Zero is accepted as the usual null value. Array casts retain the existing
shallow check; they do not inspect every element's type.

## Identity and visibility

A class's identity is its canonical defining in-game source path and its name.
Classes with identical fields are distinct when either component differs. A
definition in an included header belongs to that header; inheritance preserves
the original identity. A declaration produced by a macro belongs to the source
where that macro is expanded.

Changing field order or types does not change nominal identity. Moving a class
to another file does. Line numbers, include spelling after path normalization,
and compilation-local source IDs are not part of the identity. Defining a class
in a host source outside the mudlib is rejected because it has no in-game source
identity.

An unqualified name must select one visible class. If different inherited files
declare classes with the same name, using that name is ambiguous. Multiple
inheritance paths to the same definition are allowed. Conflicting definitions
of the same identity within one compiled program are rejected. Repeating a local
definition is also an error, even if its fields match.

Known incompatible scalar class types are rejected in assignments, arguments, returns,
and casts. A conditional expression combining different nominal class types is
also rejected: the primitive union representation cannot describe that result.
An explicitly `mixed` expression still uses the existing mixed-type rules.

Each compiled function, including closures and inherited functions, retains its
defining class scope. Recompiling another program does not replace those
definitions. Member access uses the definition retained by the executing code.

## Construction and members

```c
class position origin = new(class position, x: 4);
mixed alias = origin;
alias.y = 9;                    // origin.y is now 9
origin.next = new(class position);
origin.next.x++;
```

`new(class Name, field: value, ...)` initializes omitted fields to zero and
evaluates the supplied values once each, in written order. Duplicate names,
unknown fields, and statically incompatible values are compile errors. Runtime
checks also apply when a value's actual type was hidden by `mixed`.

Member syntax composes with calls, indexing, and other members. A statically
typed receiver must have the named field. A `mixed` receiver selects a visible
definition by its actual nominal identity; access fails at runtime if the
executing code has no definition for that identity, the field is absent, or the
receiver is not a class instance. Access through zero is an error.

Assignment shares an instance. Equality and mapping keys use instance identity,
so two separately constructed records with identical values are unequal.
`classp(instance)` returns `1`; `typeof(instance)` returns `9`;
`arrayp(instance)` is false. Positional indexing
and arithmetic on instances are unsupported. `sizeof(instance)` returns zero,
like other values without a defined size.

Member assignment, compound assignment, and prefix/postfix `++` and `--`
evaluate their receiver once. Member assignment resolves the receiver before
its right-hand side. Efun output arguments also accept members:

```c
sscanf(text, "%d", origin.x);
```

The receiver is evaluated before the call, and the output is checked and written
back afterward. Explicit `ref origin.x` remains unsupported; a variable holding
an instance can still be passed by reference.

## Runtime checks and transactions

Every field replacement checks its declared type, even if the source variable
has a misleading declared primitive type. Zero is valid for every field type.
A caught failed replacement leaves the field unchanged. This includes
construction, compound writes, and efun output writeback.

Scalar class values are checked by nominal identity in variable initialization
and assignment, function arguments and returns, casts, and typed consumption of
variables, indexed elements, and `foreach` elements. Passing through `mixed` or
changing a variable through a reference alias does not bypass these checks.
Arrays retain shallow container checks. Mutating an array stored in a field
uses normal shared-array behavior and does not recursively check element types.

An instance occupies one transactional cell. Field changes commit or abort with
the attempt; conflicting accesses use the existing retry mechanism. Garbage
collection traces all retained fields, including nested collections and captured
functions. Unreachable cycles are collected.

Access through a different compiled definition acquires a field view in the
same transaction. Fields with the same name and declared type reuse storage;
new names or types start at zero. Field order does not affect storage. Views and
slots remain until the instance is collected. Removing and later reintroducing
a field with its previous name and type can therefore recover its old value;
type changes that later revert have the same behavior.

Saving a class currently raises an error, including when nested inside a collection;
`nosave` globals retain their normal exclusion behavior.

## Shallow copying

[`copy_class(value)`](../efun/copy_class.md) creates an independent instance of
the same nominal class. It copies every materialized view and all retained field
storage, including fields accessible only to older code. Compatible fields
continue to share storage between views within the copy. No new view is acquired
just to copy the instance.

```c
class position duplicate = copy_class(origin);
duplicate.x = 7;       // origin.x is unchanged
duplicate.next.x = 8;  // origin.next.x changes too
```

Nested reference values stay shared. Self-references still point to the original
instance; copying does not walk the reference graph. The new instance compares
unequal to the original, even before any field changes. Its allocation follows
normal transaction commit and rollback rules.

Neither copying nor [`classp(value)`](../efun/classp.md) requires a class
definition visible in the caller. This lets a generic helper copy records while
preserving their nominal identity. `copy_class(0)` is an error; `classp(0)` is
false. Ordinary arrays carry no class identity and cannot be converted back
into a class by this helper.
