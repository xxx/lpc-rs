# Variadic parameters

A final array parameter followed by `...` collects the arguments after the
fixed parameters into a new array:

```c
mixed *collect(int first, mixed *rest...) {
    return ({ first }) + rest;
}

collect(1);           // first = 1, rest = ({})
collect(1, 2, "x");   // first = 1, rest = ({ 2, "x" })
```

The collector does not count as a fixed parameter. An empty tail produces an
empty array, and a single array argument becomes one element of the collector.
Use [argument spreading](argument_spreading.md) to pass its elements separately:

```c
void create(mixed *args...) {
    setup(args...);
}
```

## Declarations and types

The collector must be last, have an array type, and have neither `ref` nor a
default value. `string *names...`, `int *numbers...`, and other array types
are supported. `*args...` means `mixed *args...` unless
[`strict_types`](pragmas.md#strict_types) requires an explicit type.

The declared type governs uses of the collector inside the function. As with
other array values, collected elements are not individually type-checked:
`string *names...` does not reject an integer in the tail. Use `mixed *` for
heterogeneous arguments, and validate elements when necessary.

The collector is an ordinary local variable: it can be reassigned, captured by
a closure, or passed to another function using `ref`. Each call creates a fresh
collector, while nested arrays, mappings, and objects keep their identities.

## Argument counts and defaults

Named collectors retain the same call rules as the bare `...` form:

- Direct and simul-efun calls require the fixed parameters, except those with
  defaults. A spread defers that count check to runtime.
- `varargs` allows omitted fixed parameters to read as zero; declared defaults
  still apply. The collector itself always receives an array.
- Function-pointer and cross-object calls keep their existing permissive count
  rules: missing fixed parameters use their defaults or zero, and extra
  arguments go into the collector.

For example, `int count(int n = 7, mixed *rest...)` accepts no arguments:
`n` is `7` and `rest` is empty. Passing `0` supplies `n` explicitly.

## Closures and the bare form

Closures use the same syntax inside their parameter brackets:

```c
function collect = (: [int first, mixed *rest...] ({ first }) + rest :);
```

With a named collector, `$1`, `$2`, etc. may refer only to explicitly declared
fixed parameters. Index the collector to reach the remaining arguments.

The existing declarations `f(...)` and `f(int n, ...)` still create the implicit
`mixed *argv` local. A named collector replaces that implicit local; `argv`
is then available as an ordinary name, including as the collector's name.
Each nested callable determines its own reservation of `argv`.

## FluffOS porting

FluffOS also supports named collectors. In lpc-rs the collector must explicitly
be an array, and a named tail alone does not make the fixed prefix optional on
direct calls. Add `varargs` or defaults where needed. See the pinned
[driver comparison](../driver-comparison.md#fluffos) for the reviewed differences.
