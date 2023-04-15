# `function` variables in lpc-rs

Let's differentiate between functions that you write in your LPC code,
and variables of the `function` type, which, really, are pointers to functions. This
document is talking about the latter (the former can be found in anatomy_of_a_program.md).

We take the position that functions are really just pieces of code that you can
call with arguments, and return a value, maybe with a receiver in context.
The source or format of the definition should not matter - if it's callable, 
we're calling it a `function` (even if it's a procedure... don't be pedantic).

Breaking it down by syntax, functions can take the following forms:

* `function_name` - A reference to a static function defined in the same file,
  a simul_efun, or an efun (checked in that order). This is a shorthand form
  of the next variant.

* `&function_name()` - This is equivalent to the above. No arguments are put into
  place in the declaration, but you can still call the function with arguments.

* `&function_name(arg1, arg2)` - This is a reference to a static function
  defined in the same file, a simul_efun, or an efun (checked in that order).
  The arguments are put into place in the declaration, and you can call the function
  with additional arguments, or without, and the arguments will be filled in from the declaration.

* `&function_name(arg1,,arg3)` - This is a reference to a static function
  defined in the same file, a simul_efun, or an efun (checked in that order).
  Some arguments are put into place in the declaration, while holes are left for spots for
  arguments that are filled when you call the pointer.
  In the example `f = &function_name(arg1,,arg3); f("foo");`, `foo` will be put into
    the second argument, where it was empty in the declaration. This is referred to
  as "partial application".

* `&(object)->function_name()` - This is a reference to a function defined in
  another object. The object is put into place in the declaration, and you can call
  the function with additional arguments, or without, and the arguments will be filled in
  from the declaration.

* `&(object)->function_name(arg1,,arg3)` - This is a reference to a function defined in
  another object. The object is put into place in the declaration, and you can call the function
  with additional arguments, or without, and the arguments will be filled in from the declaration.
  Some arguments are put into place in the declaration, while holes are left for spots for
  arguments that are filled when you call the pointer.
  In the example `f = &(object)->function_name(arg1,,arg3); f("foo");`, `foo` will be put into
    the second argument, where it was empty in the declaration. Again, this is referred to
  as "partial application".

* `&->function_name()` - This is a reference to a function defined in another object.
  The object is filled-in when the function is called, with the first argument.
  Subsequent arguments are passed to the function. Partial application can be used.

* `(: function_name() :)` - A closure. Closures are functions that are defined
  inline, and capture any variables they reference from their environment.
  * When called, closures without an explicit `return` will return their last 
    expression result, even if it's `void`, in which case `0` will be returned. 
  * Closures _always_ return a value when called, treated as type `mixed` (this might get stricter in the future).
  * Closures can use `$1`, `$2`, etc. to refer to the arguments passed to the function, based on their position.
    For security purposes, `$64` is the maximum.
  * Closures can optionally declare their arguments within `[]` at the start of the function body, e.g.
  `(: [int x, int y] x + y :)`. This is optional, but recommended.
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
    
Visibility is in the context of the object where the variable is created. If a `private` function is accessible to an object,
that object can create a `function` variable pointing to it, and it will be callable from within any other object.

All `function` variables can be used in compositions with the `@` operator, or the `compose` efun.

`function` variables cannot be partially applied via the `&` syntax (it only works when first declaring them),
but the `papplyv` efun is able to do this. This is only because fiddling around with the parser to make it work 
turned into a little box of hell.