# function_object

`object function_object(function f)`

Return the live receiving object of a function pointer without invoking the
function or loading or initializing an object.

Local pointers return their target object, including inherited functions and
closures. Bare local pointers (`foo` or `&foo()`) use their owner as the receiver.
Efun pointers also return their owner. Simul-efun pointers return the resident
simul-efun object if it still provides the named function.

For a dynamic pointer, return its bound object, or find an already existing object
for its bound path. Dynamic paths are relative to `/`, as when calling the pointer.
A composition returns its outer function's receiver, including a dynamic receiver
bound on the composition itself.

Return `0` for an unbound, invalid, destructed, or unavailable receiver. An unbound
dynamic pointer does not fall back to its owner. A non-function argument is an error.

### Examples

```c
function_object(foo) == this_object();        // 1, for a local foo
function_object(&foo()) == this_object();     // 1
function_object(write) == this_object();      // 1
function_object(&target->foo()) == target;    // 1
function_object(&->foo());                    // 0: receiver still unbound
```

`function_info(f)` exposes the owner separately and preserves a bound path string.

### See also

`function_info`, `function_name`, `functionp`, `find_object`
