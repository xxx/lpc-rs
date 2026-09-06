# Argument spreading

`f(a, xs...)` passes `a`, then each element of `xs` as its own argument:

```c
int add(int a, int b, int c) { return a + b + c; }

int create() {
    int *xs = ({ 2, 3 });
    return add(1, xs...);      // add(1, 2, 3) == 6
}
```

A spread may sit in any argument position, any number of times, and works
for every kind of call: a local or inherited function, an efun, a simul
efun, `ob->f(...)`, `call_other(ob, "f", ...)`, a function-typed variable,
and a chained call `make()(...)`. An empty array spreads to nothing. Extra
arguments past the callee's parameters land in `argv` as usual.

## Rules the compiler enforces

- The operand's type is an array or `mixed`; anything else is
  "`...` spreads an array, not int".
- Arguments before the first spread are checked by position as usual;
  from the spread on, the argument count and per-position types are left
  to the runtime.
- A `ref` argument cannot follow a spread (its position is unknown), and
  an efun's implicit by-reference argument (a bare variable where `sscanf`
  wants an lvalue) is only recognised before the first spread.
- `catch(x...)`, `sizeof(x...)`, and spreading `call_other`'s receiver or
  function name are compile errors; so is a spread in a function pointer's
  partial argument list, `&f(x...)`.

## Rules the runtime enforces

- A non-array value in a spread is the runtime error
  "cannot spread int: `...` takes an array".
- A spread element, or any value after a spread, that lands on an efun's
  by-reference parameter is "argument N of `sscanf` must be passed by
  reference".
