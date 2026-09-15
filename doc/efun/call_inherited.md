# call_inherited

`mixed call_inherited(string name, ...)`

Call an inherited function on `this_object()`, bypassing a local override.
Return its result, or `0` when no accessible inherited definition exists.
A `void` function returns `0`.

## Names and visibility

- `"foo"` and `"::foo"` select the same definition as `::foo()`.
- `"parent::foo"` selects the same definition as `parent::foo()`, where
  `parent` is an inherit alias declared in the calling source program.
- Later inherits take precedence, including definitions they inherited.
- Public and protected (`static`) functions are callable. Private inherited
  functions return `0`, without falling back to an earlier inherit.
- Unknown aliases, malformed names, and missing functions return `0`.
  Local functions, efuns, and simul-efuns are not fallback targets.

Inherited code keeps its own parents and aliases. Closures and pointers to this
efun use the source scope where they were created. Calls use the current
object's compiled program without loading or initializing parent objects.
The inherited function operates on the current object's globals, including
when that object is a clone.

A direct call to this efun preserves the caller's `previous_object()`, as a
direct `::foo()` call does. The inherited function sees the calling LPC function
and program through `calling_function()` and `calling_program()`.

## Arguments and errors

Arguments are evaluated before lookup, even when the function is absent.
Use `function_exists()` as a guard to skip argument evaluation.

Existing functions check argument counts and types at runtime. Default
parameters, `varargs`, ellipsis, and array spreading work as in a direct call.
Arguments are passed by value; a target declaring a `ref` parameter raises an
error and must be called directly.

A non-string name raises an error. Errors from an existing function propagate
to the caller and can be caught with `catch()`.

## Examples

```c
inherit "/std/object" parent;

void create() {
    call_inherited("parent::create");
    if (function_exists("parent::notify")) {
        call_inherited("parent::notify", sprintf("%s is ready", file_name(this_object())));
    }
}
```

Use `call_inherited("foo", value)` to search all parents in inheritance order.
An unresolved direct `::foo()` remains a compile error, including inside an
existence guard; `call_inherited()` provides the optional call.

## See also

`function_exists`, `call_other`, `inherit_list`, `previous_object`
