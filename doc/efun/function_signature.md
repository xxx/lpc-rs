# function_signature

`mapping function_signature(string name, object ob)`

Inspect the compiled definition of `name` in `ob`, without calling it,
initializing its object, loading a path or evaluating default expressions.
Both arguments are required. The definition comes from the transaction-visible
program, so clones and prototypes follow successful recompilation.

The returned mapping contains:

| Field | Value |
| --- | --- |
| `name` | The definition's unqualified name. |
| `program` | Its defining source's in-game path, retaining the source extension. |
| `return_type` | The declared return type as a native type string. |
| `parameter_types` | Declared fixed parameter types, in order, as type strings. |
| `parameter_refs` | Parallel integer flags: `1` for a `ref` parameter, `0` otherwise. |
| `num_args` | The fixed parameter count, including parameters with defaults. |
| `num_default_args` | How many fixed parameters have declared defaults. |
| `varargs` | `1` for a `varargs` declaration, `0` otherwise. |
| `ellipsis` | `1` for bare `...` or a named variadic collector, `0` otherwise. |
| `visibility` | `"public"`, `"protected"` or `"private"` in this object, including inheritance restrictions. |
| `nomask` | `1` when the function declaration is `nomask`, `0` otherwise. |

A named variadic collector is excluded from the fixed count and parameter
arrays. Parameter names, default expressions and collector types are not
retained in this metadata. The counts describe the compiled declaration, not
partially applied arguments or the number of arguments supplied to a call.
For call rules, see [variadic parameters](../lpc/variadic_parameters.md) and
[function values](../lpc/function_type.md).

Type strings use native spelling, including array stars; a class
includes its nominal name and defining source. Types are retained regardless of
`save_types` pragmas. The mapping and both arrays are fresh; editing them does
not change the compiled definition or a later result.

An unqualified name selects the resolved definition, including a local override.
Qualified names such as `"parent::method"` and `"::method"` use the same inherited
lookup as [`function_exists`](function_exists.md): the calling source's scope
for self, and the target's root scope for another object. An inherited efun
pointer or closure keeps the scope that created it. A failed qualified lookup
does not fall back to an unqualified definition.

Self can inspect its private and protected functions. Another object exposes
only public bindings. Missing or hidden definitions, null targets and destructed
targets return the configured null value. A null target does not select self;
pass `this_object()` explicitly. A non-string name or a non-object, non-null
target is an error. A path string is not accepted. Efuns and simul-efuns are not
looked up through the target unless that function is actually defined in its
compiled program. Generated function names are implementation details.

```c
int add(int number, string label = "default") { return number; }

void create() {
    mapping info = function_signature("add", this_object());
    info["return_type"];      // "int"
    info["parameter_types"];  // ({ "int", "string" })
    info["parameter_refs"];   // ({ 0, 0 })
    info["num_args"];         // 2
    info["num_default_args"]; // 1
}
```

Use [`functions`](functions.md) to enumerate names and
[`function_info`](function_info.md) to inspect pointer bindings.
