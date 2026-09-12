# function_name

`string function_name(function f)`

Return the function pointer's bare function name, without a receiver, argument
list, or partial-application marker. Bare names (`function f = foo`), `&foo()`,
and partial applications of the same function return the same name.

This reads the name stored in the pointer without invoking it, looking up its
receiver, or loading an object. The name remains available when the owner or
receiver is destructed, a dynamic receiver is unbound or invalid, or a simul-efun
target is unavailable. It does not establish that the function can be called.

Closures, operators, and compositions return their generated names, which are
implementation details and may change when code is compiled. A non-function
argument is an error.

The result is the same as `function_info(f)["name"]`, without allocating the
metadata mapping and its arrays. Use `function_description(f)` for a formatted
representation including the receiver and masked argument positions.

### Examples

```c
function_name(add);               // "add"
function_name(&add());            // "add"
function_name(&add(10, ));        // "add"
function_name(&target->add());    // "add"
function_name(&write("secret"));  // "write"
function_name(&->query_name());   // "query_name"

filter(query_call_outs(), (: function_name($1[1]) == "cancel_me" :));
```

### See also

`function_description`, `function_object`, `function_info`, `functionp`, `function_exists`
