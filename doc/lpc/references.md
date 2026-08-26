# By-reference parameters

A parameter declared `ref` is the caller's variable, not a copy:

```c
void inc(int ref x) { x++; }

int create() {
    int y = 1;
    inc(ref y);      // y is now 2
    return y;
}
```

`ref` qualifies a parameter (`type ref name`); the argument is written
`ref name`, where `name` is a variable of the calling program — a local, a
parameter, a captured variable, or a global. Only a variable can be passed
by reference: `ref a[i]`, `ref o->v`, and `ref f()` do not parse.

Inside the callee, assignment, `++`/`--`, and compound assignment write
through to the caller's variable, and a read sees any write the caller made
in the meantime. A `ref` parameter can be passed on by `ref` to another
call. A closure created in the callee that captures a `ref` parameter
captures the caller's variable, and that alias outlives the call — the same
rule closures follow for every capture.

## Rules the compiler enforces

For a direct call to a function whose declaration is visible (the program's
own functions, inherited ones, simul_efuns, efuns):

- a `ref` parameter must receive a `ref` argument: `argument N of `f` must be
  passed by reference`;
- a plain parameter refuses one: ``f` does not take argument N by reference`;
- types check between the variable's declared type and the parameter's.

A `ref` parameter cannot have a default value, cannot be in the range
`varargs` makes optional, and cannot be the `...` tail. A closure cannot
take a `ref` parameter.

## What is not supported

- Through a function pointer: `fp(ref y)` is a compile error, and calling a
  `ref`-taking function through any pointer (`call_out`, `input_to`, a
  closure, `evaluate`) is a runtime error naming the function.
- Across objects: `o->f(ref y)` and `call_other(o, "f", ref y)` are compile
  errors.
- In partial application: `&f(ref y)` does not parse.
- To an element: `ref a[i]` does not parse. Arrays and mappings are already
  shared by identity.

A mismatch the compiler could not see — an override with different
`ref`-ness, a `call_other` reaching a `ref` function — is a runtime error
at the call, so a `ref` parameter never silently receives a copy.

## Efuns take lvalues implicitly

`sscanf(str, fmt, a, b)` writes into `a` and `b` without `ref`, as in every
driver: the efun's prototype marks those positions, and the compiler
requires a variable there (`argument N of `sscanf` must be a variable`).
Writing `ref` explicitly is accepted and identical.

## Differences from other drivers

LDMud's `&x` (a reference as a runtime value, with an unaware callee) does
not exist here; the MudOS/FluffOS `ref` form is the only one. FluffOS allows
`ref` through function pointers; this driver does not.
