TODO: finish this

```c
inherit "/std/object";
inherit "/lib/container";

string name; // public - directly accessible by inheritors
protected float shoe_size; // protected - directly accessible by inheritors
private int volume // private - only accessible by this object. Completely invisible to inheritors.

void create() {
    ::create();
    set_name("foo");
    shoe_size = 12.5;
    volume = 9001;
}

private void set_name(string n) {
    name = n;
}
```

An `inherit` path can be a constant string expression, including concatenation
of macros defined earlier in the source or an included header:

```c
#define ARMOUR "/std/armour"
inherit (ARMOUR + "/base");
```

Parentheses are optional, and a namespace can follow the expression:
`inherit ARMOUR + "/base" armour;`. The expression must resolve to a string
at compile time; runtime variables and function calls are rejected.

notes:

- decls can be anywhere in the body (globals are hoisted for codegen, but not for semantic analysis)
- `foreach` accepts `in` or `:` before the collection; see [foreach loops](foreach.md)
- globals are 'global' only to the program, and cannot be accessed from other programs. they just outlive function invocations.
- globals are initialized when the object is created, prior to calling `create`, (which happens automatically)
- `nosave` or `static` on a global excludes it from object save/restore without changing visibility; see [non-persistent globals](nosave.md)
- a parameter declared `ref` is the caller's variable; see [references](references.md)
- `f(a, xs...)` passes each element of `xs` as its own argument; see [argument spreading](argument_spreading.md)
- a final parameter such as `mixed args...` or `mixed *args...` collects the remaining arguments into an array; see [variadic parameters](variadic_parameters.md)
- a function declared without a return type is `mixed`, and so is a parameter declared without a type (`start_mail(qroom)`); [`#pragma strict_types`](pragmas.md#strict_types) makes either an error from its line to the end of the file that declares it, and an `#include`d file is outside it unless it declares the pragma itself
- a call by name from any of the object's code, inherited code included, reaches the object's last definition of that name: a redefinition in the child intercepts the parent's own calls (`private` or `static` on the redefinition included), a `private` function is never intercepted, and of two inherited definitions the later inherit's wins
- `::f()` calls the inherited `f`, `name::f()` the one of the parent inherited as `name` (`inherit "/x" name;`), and `efun::f()` the efun; each looks only there, so a name that program lacks is a compile error
- a redefinition of an inherited function declares the same number of parameters unless the inherited one is `varargs`; the parent's calls run the redefinition with the arguments they pass, missing ones 0
- `#include "x.h"` looks beside the including file, then in the system include dirs; `<x.h>` looks only in the system dirs
- `#define` supports object macros, function macros, and argument stringification with `#`; see [macros](macros.md)
- a source file that is not UTF-8 is read as Latin-1, with a warning

## Function calls

A nonempty argument list may end with one trailing comma:

```c
set_name("Ada",);
room->announce("Hello",);
callback(first, rest...,);
callbacks[0](value,);
```

The comma adds no argument and does not change evaluation order, argument
checking, defaults, or [spreading](argument_spreading.md). It also works after
a [reference argument](references.md), as in `increment(ref count,)`, and in
inherited and explicit efun calls such as `::create(value,)` and
`efun::sizeof(text,)`.

Empty calls use `f()`. Empty argument slots such as `f(,)` or `f(1,,2)` are
invalid. Function declarations and definitions do not allow trailing commas in
their parameter lists.

In [partial application](function_type.md), `&f(1,)` still leaves an unbound
argument slot after the bound `1`; it constructs a function value rather than
calling `f`.

## Parameter defaults

Fixed parameters can declare values to use when their arguments are omitted:

```c
int total(int first, int second = 2, int third = 3) {
    return first + second + third;
}
```

Defaults must form a trailing block: once a fixed parameter has a default,
every later fixed parameter must also have one. This applies to function
definitions, including `varargs` functions, and to declared closure parameters.
For example, `void f(int first = 1, int second) {}` is a compile error.

A final [variadic collector](variadic_parameters.md) or bare `...` can follow
defaulted fixed parameters; it is separate from that block and has no default.
`varargs` can still omit fixed parameters before the block, using their usual
typed defaults.

An explicit `0` or `nil` is a supplied argument and does not request the declared
default; the usual type checks apply. Default expressions run only for omitted
arguments.

## Function prototypes

Functions can be called before their definition, so prototypes are unnecessary.
Inherited code can also use definitions supplied by the receiving object or a
sibling inherit; see [receiving-object functions](inheritance.md#functions-supplied-by-the-receiving-object).
For source compatibility, the compiler accepts prototypes with named or unnamed
parameters, including a mixture of both:

```c
void start_shell(int);
int search(string, string text, int);
void update(int ref, object *);
void collect(int, mixed *...);
```

Each prototype produces a warning that it is ignored, unless warnings are
disabled. The actual function definition supplies the return type, parameter
types, argument count, defaults, and reference requirements used to check calls.
A prototype alone does not make a function callable, and its signature is not
checked against the definition.

An unnamed parameter must have an explicit type. Arrays keep their `*`, and a
variadic collector requires `mixed` or an array type even when its name is omitted.
Parameter defaults require a name. `f(void)` and `f()` both specify an empty
parameter list.

Function definitions and declared closure parameters require names:

```c
void start_shell(int mode) { }
function callback = (: [int mode] start_shell(mode) :);
```
