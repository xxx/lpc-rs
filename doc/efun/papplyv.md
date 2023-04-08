# papplyv

`function papplyv(function f, mixed *args)`

Partially apply a function to a list of arguments. This is useful when you want
to call a function with a variable number of arguments, and don't know exactly
what they are until runtime.

### Examples

```c
function f = (: $1 + $2 :);

function g = papplyv(f, ({ 1, 2 }));
g(); // 3

function h = papplyv(f, ({ 1 }));
h(3); // 4

function i = papplyv(f, ({ }));
i(1, 2); // 3

function j = &dump("hello");
function k = papplyv(j, ({ "world" }));
function l = papplyv(k, ({ "!!!" }));
l("WOW"); // "hello" "world" "!!!" "WOW"
```
