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

notes:

- no prototypes necessary
- decls can be anywhere in the body (globals are hoisted for codegen, but not for semantic analysis)
- globals are 'global' only to the program, and cannot be accessed from other programs. they just outlive function invocations.
- globals are initialized when the object is created, prior to calling `create`, (which happens automatically)
- a parameter declared `ref` is the caller's variable; see [references](references.md)
- `f(a, xs...)` passes each element of `xs` as its own argument; see [argument spreading](argument_spreading.md)
- a function declared without a return type is `mixed`, and so is a parameter declared without a type (`start_mail(qroom)`); `#pragma strict_types` makes either an error from its line to the end of the file that declares it, and an `#include`d file is outside it unless it declares the pragma itself
- a call by name from any of the object's code, inherited code included, reaches the object's last definition of that name: a redefinition in the child intercepts the parent's own calls (`private` or `static` on the redefinition included), a `private` function is never intercepted, and of two inherited definitions the later inherit's wins
- `::f()` calls the inherited `f`, `name::f()` the one of the parent inherited as `name` (`inherit "/x" name;`), and `efun::f()` the efun; each looks only there, so a name that program lacks is a compile error
- a redefinition of an inherited function declares the same number of parameters unless the inherited one is `varargs`; the parent's calls run the redefinition with the arguments they pass, missing ones 0
- `#include "x.h"` looks beside the including file, then in the system include dirs; `<x.h>` looks only in the system dirs
- a source file that is not UTF-8 is read as Latin-1, with a warning
