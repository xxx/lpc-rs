# Class declarations and nominal types

The compiler accepts class definitions and named class types. Runtime instance
construction, member access, copying, and persistence are not implemented yet.
Declarations alone do not make a mudlib that uses class instances runnable.

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
definitions. This metadata is the basis for future runtime field views.
