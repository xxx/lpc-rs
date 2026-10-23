# origin

`string origin()`

Return how the currently executing function was entered. This describes the
immediate dispatch mechanism, independently of the caller's identity.

| Result | Entry mechanism |
|---|---|
| `"local"` | A direct named call, including inherited calls and `call_inherited`. |
| `"call_other"` | `->` or `call_other`, including calls to the same object and collection receivers. |
| `"simul_efun"` | A simul-efun called by name. |
| `"function_pointer"` | An explicit call through a named, bound, dynamic, simul-efun, or efun pointer. |
| `"closure"` | An explicit call to an anonymous closure. |
| `"efun"` | A synchronous callback requested by an efun, such as `map`, `filter`, `reduce`, `sort_array`, or an object's `id` called by `present`. |
| `"driver"` | A driver apply, object initialization, command or parser handler, `parse_string` action, or input callback. |
| `"call_out"` | A scheduled `call_out` callback. |

The dispatcher takes precedence over the callback's representation. A closure
called directly reports `"closure"`; the same closure called by `map` reports
`"efun"`, and scheduled through `call_out` reports `"call_out"`. Driver applies
for permissions, lifecycle, commands, and parsing report `"driver"` even when
an efun caused the driver to invoke them. The `id` callback requested by
`present` reports `"efun"`.

[`evaluate`](evaluate.md) uses ordinary pointer-call classification. Inside its
callback, `origin()` returns `"function_pointer"` for a named function and
`"closure"` for a closure.
A composition invokes its component functions through ordinary pointer calls;
each component reports its own immediate entry mechanism.

```c
string probe() { return origin(); }

mixed *example() {
    function named = probe;
    function anonymous = (: origin() :);
    return ({ probe(), this_object()->probe(), named(), anonymous(),
              map(({ 0 }), (: origin() :))[0] });
    // ({ "local", "call_other", "function_pointer", "closure", "efun" })
}
```

A nested call receives its own origin. Returning or catching an error restores
the enclosing function's origin; suspension and transaction retries preserve
the classification. Calling `origin` through a pointer inspects that pointer's
entry, so `evaluate(origin)` returns `"function_pointer"`.

Use [`calling_object`](calling_object.md), [`calling_function`](calling_function.md),
and [`calling_program`](calling_program.md) to identify callers. Origin does not
authenticate an object or program: trusted-source policies must also inspect
the relevant identities. [`previous_object`](previous_object.md) continues to
count object crossings independently of these categories.
