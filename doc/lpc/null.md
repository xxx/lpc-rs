# Null values and `nil`

`nil` represents absence. Its value depends on the VM's startup setting
`NULL_MODE`, which defaults to `zero`:

| Behavior | `zero` | `distinct` |
|---|---|---|
| Value of `nil` | Integer `0` | A separate nil value |
| `nil == 0` | True | False |
| `nil == 0.0` | True | False |
| `!nil` | `1` | `1` |
| `intp(nil)` | `1` | `0` |
| `nilp(nil)` | `1` | `1` |
| `nilp(0)` | `1` | `0` |
| `typeof(nil)` | `1` (`T_INT`) | `10` (`T_NIL`) |
| Missing mapping entry | `0` | `nil` |
| Missing object or function result | `0` | `nil` |
| Uninitialized `int` | `0` | `0` |
| Uninitialized `float` | Integer `0` | Float `0.0` |
| Other uninitialized values | `0` | `nil` |

Set `NULL_MODE=distinct` in the environment or the configuration file passed
with the driver's `-e` option or compiler's `-c` option. `LPC_NULL_MODE` is also
accepted. Only `zero` and `distinct` are valid. The setting applies to the whole
VM, including inherited and recompiled code, and cannot change while it runs.
It is independent of type-checking pragmas.

## Types and defaults

In distinct mode, `nil` fits `mixed`, strings, bytes, objects, mappings,
functions, classes, and arrays. It does not fit scalar `int` or `float`.
Numeric `0` is an ordinary integer and loses its special exemption from type
checks. Arithmetic on nil raises an error. Use `value == nil` to test absence;
`!value` also accepts numeric zero and `0.0`.
[`nilp(value)`](../efun/nilp.md) tests the selected null value in either mode;
it rejects float `0.0`, including in zero mode.
Numeric [equality](comparisons.md) makes `0 == 0.0` true in both modes; it does
not make float zero an absence value for type checks.

```c
mixed absent = nil;
mixed count = 0;
string name = nil;

if (absent == nil) { /* no value */ }
if (count == 0) { /* an actual numeric zero */ }
```

Type-aware defaults apply to locals, globals, class fields, omitted fixed
parameters, and functions returning without a value. A `void` result is nil in
distinct mode. Explicit parameter defaults apply only when the argument is
omitted; supplying nil or zero does not request the default. Captured variables
and parameters follow the same rules as ordinary variables and parameters.

Existing type checks retain their scope: `mixed` expressions may flow into
narrower variables without a general runtime assignment check. Cast assertions,
function-pointer argument checks, class-field checks, and
[`store_variable`](../efun/store_variable.md) enforce the selected null rules.

A destructed object behaves as the selected null value for truth, equality,
type predicates, casts, JSON, and save output. Transactional destruction is
visible immediately within the attempt that performed it.

## Containers and efuns

A missing mapping key yields nil in distinct mode. A present key can also hold
nil: assigning nil does **not** delete the key. Use `m_delete` to remove it
and [`has_key(mapping, key)`](../efun/has_key.md) to test membership. Nil and
integer zero are separate mapping keys in distinct mode.

`allocate(n)` fills its elements with the selected null value. Failed lookups
and absent reference results, such as `find_object`, `environment`, `read_file`,
`regmatch`, `parse_string`, and missing `call_other` methods, return it too.
Optional reference arguments use nil where zero mode uses `0`.

Numeric results keep their established contracts: predicates and failure status
codes still return integers; `strindex` still returns `-1` for no match;
`catch` still returns integer `0` on success; out-of-range string and bytes
indexing still returns integer `0`. Nil does not replace these numeric results.
`to_string(nil)` produces `"nil"` in distinct mode; numeric conversions reject it.

## Persistence and JSON

Saved `0` always restores as integer zero, regardless of the destination's type.
Saved `nil` stays nil in distinct mode. In zero mode it becomes `0`, and the
restore emits one warning with the number of converted nil values and, for file
restores, the path. The count includes nested values and mapping keys.
[`restore_value`](../efun/restore_value.md) uses the same conversion rules for
serialized strings. Warnings are delivered only if the task commits; a retry or
aborted attempt does not duplicate them.

Collapsing nil and zero mapping keys can merge entries; later entries win.
Restore does not infer that an old saved zero meant absence. Class-record
validation still rejects values incompatible with their stored field types.
See the [save format](../save-format.md) for the complete grammar.

JSON `null` decodes to the selected null value. Distinct nil encodes as JSON
`null`; integer zero encodes as `0`. JSON booleans retain the existing mapping:
`false` becomes `0` and `true` becomes `1`.
