# `function` variables in lpc-rs

Let's differentiate between functions that you write in your LPC code,
and variables of the `function` type, which, really, are pointers to functions. This
document is about the latter (the former can be found in anatomy_of_a_program.md).

We take the position that functions are really just pieces of code that you can
call with arguments, and return a value, maybe with a receiver in context.
The source or format of the definition should not matter - if it's callable, 
we're calling it a `function` (even if it's a procedure... don't be pedantic).

Call a function variable with `f(args...)`. When a value may instead be a literal,
use [`evaluate(value, args...)`](../efun/evaluate.md): it calls functions and
returns other values unchanged.

In `f(args...)` and `&f(args...)`, an accessible `function` or `mixed` variable
takes precedence over a function with the same name. Its calls have a `mixed`
result and do not use the shadowed function's declared signature. An inherited
private variable cannot intercept either form: the compiler resolves the
function instead, or reports a visibility error if no function is available.
Code in the declaring source retains access to its own private callbacks.

Qualified calls such as `::f()` and `efun::f()`, and receiver calls such as
`object->f()`, resolve functions independently of same-named variables.

Prefer the bare function name when no arguments are being bound:
`evaluate(strindex, "hello", "ll")`. Use `&name(args)` to bind arguments,
such as `function add_ten = &add(10)`.

Breaking it down by syntax, functions can take the following forms:

* `function_name` - A reference to a function of this program (own or
  inherited), a simul_efun, or an efun (checked in that order). This is the
  preferred form when no arguments are bound.

* `&function_name()` - This is equivalent to the above. No arguments are put into
  place in the declaration, but you can still call the function with arguments.

* `&function_name(arg1, arg2)` - This is a reference to a function of this
  program (own or inherited), a simul_efun, or an efun (checked in that order).
  The arguments are put into place in the declaration, and you can call the function
  with additional arguments, or without, and the arguments will be filled in from the declaration.

* `&function_name(arg1,,arg3)` - This is a reference to a function of this
  program (own or inherited), a simul_efun, or an efun (checked in that order).
  Some arguments are put into place in the declaration, while holes are left for spots for
  arguments that are filled when you call the pointer.
  In the example `f = &function_name(arg1,,arg3); f("foo");`, `foo` will be put into
    the second argument, where it was empty in the declaration. This is referred to
  as "partial application".

* `&(object)->function_name()` - This is a reference to a function defined in
  another object. The object is put into place in the declaration, and you can call
  the function with additional arguments, or without, and the arguments will be filled in
  from the declaration. The parentheses may be dropped when the receiver is a plain variable: `&o->f()`.

* `&(object)->function_name(arg1,,arg3)` - This is a reference to a function defined in
  another object. The object is put into place in the declaration, and you can call the function
  with additional arguments, or without, and the arguments will be filled in from the declaration.
  Some arguments are put into place in the declaration, while holes are left for spots for
  arguments that are filled when you call the pointer.
  In the example `f = &(object)->function_name(arg1,,arg3); f("foo");`, `foo` will be put into
    the second argument, where it was empty in the declaration. Again, this is referred to
  as "partial application".

* `ob->name` without an argument list is the function value `&(ob)->name()`.

* `&->function_name()` - This is a reference to a function defined in another object.
  The object is filled-in when the function is called, with the first argument.
  Subsequent arguments are passed to the function. Partial application can be used.
  If the receiver has no function by that name, the call returns `0`, including
  when the pointer is used as an efun callback such as `filter` or `map`.
  Only public methods are eligible; private and protected methods return the
  selected null value, as with an ordinary `->` call. This also applies to a
  pointer to `call_other`.

* `operator(op)` - An operator as a function. `operator(+)` is the function
  that adds its two arguments exactly as `a + b` would, for every operand
  type `+` takes; it is the closure `(: $1 + $2 :)`. The operators are
  `+ - * / % > < >= <= == != & ^ | << >> []`, each taking two arguments,
  and `! ~`, taking one. Called with fewer arguments the missing ones are
  `0`, with more the extras are dropped, as for any closure.

* `&operator(op)(arg1,,arg3)` - Partial application of an operator, as for
  a named function: `&operator(-)(, 1)` subtracts 1 from its argument,
  `&operator(-)(1)` subtracts its argument from 1, and
  `&operator(==)(name)` tests its argument against `name`, which is
  evaluated when the pointer is made. Binding more arguments than the
  operator takes is a compile error. `operator` is a reserved word.

* `&f(a, , b)` where `f` holds a function value is a new value with `a`, a
  hole and `b` appended to `f`'s bound arguments.

* `(: function_name() :)` - A closure. Closures are functions that are defined
  inline, and capture any variables they reference from their environment.
  * When called, closures without an explicit `return` will return their last 
    expression result, even if it's `void`, in which case `0` will be returned. 
  * Closures _always_ return a value when called, treated as type `mixed` (this might get stricter in the future).
  * Closures can use `$1`, `$2`, etc. to refer to the arguments passed to the function, based on their position.
    For security purposes, `$64` is the maximum.
  * Closures can optionally declare their arguments within `[]` at the start of the function body, e.g.
  `(: [int x, int y] x + y :)`. This is optional, but recommended.
  * Declared parameter defaults follow the [trailing-default rule](anatomy_of_a_program.md#parameter-defaults).
  * A final `mixed` or array parameter such as `(: [mixed args...] args :)` collects all
    remaining arguments; see [variadic parameters](variadic_parameters.md).
  * Closures can be multi-line, and can use `return` to return a value, e.g.
    ```c
    (: [int x, int y] {
        if (x > y) {
            return x;
        } else {
            return y;
        }
    } :)
    ```
  * Closures capture variables from their environment, and can use them in their body.
    ```c
    function make_f() {
        int x = 5;
        int y = 10;
        int z = 15;
        return (: x + y + z :);
    }
    function f = make_f();
    f(); // 30
    ```
    
## Explicit value captures

Inside a closure, `$(expression)` evaluates the expression when that closure is
created and saves its value. Ordinary variable references keep sharing the
original variable, including later assignments:

```c
int n = 10;
function live = (: n + $1 :);
function saved = (: $(n) + $1 :);
n = 20;
live(1);  // 21
saved(1); // 11
```

Capture expressions run once per creation, in source order, even if the closure
is never called or their part of its body is later skipped by a conditional.
They do not run if execution skips creating the closure itself. Errors propagate
at creation and prevent subsequent captures from running. Transaction retries
repeat creation and its expressions; an aborted attempt discards its writes and
new closure.

The expression is checked and evaluated in the enclosing scope. The new
closure's parameters and body locals are unavailable there. Each nested `$()`
crosses one closure boundary. For example, the inner capture below reads the
argument to `factory` when `factory` creates the inner closure:

```c
function factory = (: (: $($1) + $1 :) :);
function add_ten = factory(10);
add_ten(3); // 13
```

A loop that creates `(: $(i) :)` saves that iteration's value independently.
Captures also work in global initializers and explicit parameter defaults.
A captured function can be called directly: `(: $(callback)($1) :)`.

Capturing is shallow. Arrays, mappings, objects, functions and class instances
keep their identity; changes to their shared state remain visible. Reassigning
the original variable does not replace the captured value. `$(items)[0] = 3`
and `$(record).field++` therefore modify the shared instance. The capture itself
is a value: `$(n) = 3`, `$(n)++`, and passing it directly as a `ref` argument are
rejected. A `void` expression cannot be captured, and `$()` outside a closure is
a compile error.

This syntax leaves the existing meaning of `(: name :)` and `(: name, arg :)`
unchanged: they return a function value and the last comma-expression value,
respectively. Use `name` and `&name(arg)` for named and bound pointers.

## Bound receivers and function inspection

For bound receiver forms (`&(object)->name(...)` and `object->name`), a string
receiver names an absolute object path (`"foo"` means `/foo`). If the object is
not loaded, creating the pointer loads and initializes it first, using the same
`valid_load` checks and virtual object loading as `call_other`. Loading or
initialization errors propagate to the code creating the pointer. The named
function must exist and be accessible there. Creating the pointer binds its
arguments but does not call the named function:

```c
function send = &(GMCP_HANDLER)->send(this_object(), pkg, data);
send();
```

Visibility is in the context of the object where the variable is created. If a `private` function is accessible to an object,
that object can create a `function` variable pointing to it, and it will be callable from within any other object.

All `function` variables can be used in compositions with the `@` operator, or the `compose` efun.
An unbound receiver in the left function stays open: `(&->move() @ &present("quay"))(actor)`
calls `actor->move(present("quay"))`. See [`compose`](../efun/compose.md) for argument binding.

`&f(a, , b)` builds a new value over an existing `function` variable, appending `a`, a hole and `b` after
`f`'s own bound arguments; `f`'s existing holes are left open. `papplyv(f, args)` instead fills `f`'s
existing holes from `args` first, then appends any left over, so `papplyv(&add3(, 2), ({ 1 }))` binds
`1` into the hole while `&f(1)` (over the same `f`) would leave the hole and append `1` after it.

Use [`function_object`](../efun/function_object.md) to get a pointer's receiver,
[`function_info`](../efun/function_info.md) to inspect its owner, receiver, and
partially applied arguments. [`function_name`](../efun/function_name.md) returns
the bare function name; [`function_description`](../efun/function_description.md)
includes the receiver and masked argument positions for display.
Bare local and efun pointers use their owner as the receiver;
an unbound dynamic pointer has no receiver until one is supplied.

Unfilled partial-application holes supply the selected [null value](null.md),
including distinct nil in distinct mode. A hole is a supplied argument, so it
does not trigger a declared parameter default.
