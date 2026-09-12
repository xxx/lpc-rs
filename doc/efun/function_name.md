# function_name

`string function_name(function f)`

Return the printable name of a function pointer without invoking it.
Object functions are named `"/object"->function`, with the receiving object's
in-game name, including its clone suffix. Inherited functions use the
receiving object; simul-efuns use the resident simul-efun object. Efuns use
the pointer's owner as their receiver. Bare names (`function f = foo`) and
`&foo()` print identically; a bare local pointer's receiver is its owner.

Partially applied pointers have a leading `&` and an argument list: `?`
marks a bound value and `_` marks a hole. These argument values are never printed.

Dynamic pointers use `->function` until their receiver is bound, then
`"/object"->function` for an object or `"path"->function` for a path.
The receiver is separate from the displayed argument list; method argument
values are masked. Naming one does not resolve or load its receiver.
Closures, operators, and composed functions expose their generated names;
these names are implementation details and may change when code is compiled.

Return `0` for a destructed receiver, an invalid bound dynamic receiver,
or an unavailable simul-efun target.
A non-function argument, or a result exceeding the maximum string length,
is an error.

### Examples

```c
// In /example.c, with a local function add(a, b):
function_name(&add());            // "\"/example\"->add"
function_name(&add(10, ));        // "&\"/example\"->add(?,_)"
function_name(&write());          // "\"/example\"->write"
function_name(&write("secret"));  // "&\"/example\"->write(?)"
function_name(&->query_name());   // "->query_name"
```

### See also

`function_object`, `function_info`, `functionp`, `function_exists`, `calling_function`, `papplyv`
