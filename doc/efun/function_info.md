# function_info

`mapping function_info(function f)`

Inspect a function pointer without invoking it, loading objects, or initializing
its receiver. Bare names (`function f = foo`) and `&foo()` return the same metadata.

The returned mapping has these fields:

| Field | Value |
| --- | --- |
| `name` | The function's bare name; closures, operators, and compositions use generated names. |
| `kind` | `"local"`, `"efun"`, `"simul_efun"`, `"dynamic"`, or `"composed"`; `"local"` includes closures and operators. |
| `owner` | The object that created the pointer, or `0` if destructed. |
| `receiver` | The receiving object, a bound dynamic path string, or `0` if unbound, invalid, or destructed. |
| `receiver_bound` | `1` if the receiver is supplied by the pointer; `0` if it must be passed when calling it. This does not establish that the receiver is valid or alive. |
| `arguments` | An array of partially applied argument values, with `0` in holes. |
| `bound` | An array parallel to `arguments`: `1` for a bound value, `0` for a hole. |
| `components` | `({ outer, inner })` for `outer @ inner`; an empty array otherwise. |

A local pointer uses its target object as the receiver. For a bare local pointer
this is also its owner; inherited functions use the receiving object rather than
the inherited program. Efun pointers use their owner as the receiver. Simul-efun
pointers use the resident simul-efun object, or `0` if the target is unavailable.
Inspecting a pointer in another object does not change its owner or receiver.

Dynamic receiver slots are separate from `arguments`. A bound path stays a string
even if its object is loaded; use `function_object(f)` to obtain an existing object.
An unbound dynamic pointer has no receiver yet and does not fall back to its owner.

A composition reports the outer function's receiver, including a dynamic receiver
bound on the composition itself. Its `arguments` are the values applied to the
composition; inspect each member of `components` to obtain that function's own
bindings. Component functions and the reserved receiver are excluded from
`arguments` and `bound`.

The mapping and its arrays are fresh containers. Replacing an argument slot or a
bound flag in the result does not change the pointer. Array and mapping values
inside argument slots retain their usual reference semantics. Lexically captured
closure variables are not partially applied arguments and are not included.

A destructed owner or receiver does not prevent inspection of the remaining
metadata. A non-function argument is an error.

### Examples

```c
function f = &foo(10, , 0);
mapping info = function_info(f);
info["arguments"]; // ({ 10, 0, 0 })
info["bound"];     // ({  1, 0, 1 })

function bare = foo;
function_info(bare)["receiver"] == this_object(); // 1
function_info(write)["receiver"] == this_object(); // 1
```

### See also

`function_object`, `function_name`, `function_description`, `functionp`, `papplyv`, `compose`
