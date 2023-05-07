# lpc-rs supported types

* `int`
  - Run-of-the-mill signed integers, 64 bits by default. This size is ostensibly
    changeable in `lpc-rs-core`, but no testing is done on different sizes. 
  - `0` doubles as the null value for LPC, meaning it's always valid to assign it to a variable,
    and it's returned by functions with non-`int` return types to indicate the absence of a value.
  - `1` and `0` are very commonly used as boolean values in the language.
  - `int`s are value types in LPC, meaning they are copied when assigned, or passed to
    functions.
  - Examples:
    - `int x = 5;`
    - `int y = x;`
    - `int z = x + y;`

* `float`
    - IEEE 754 double-precision floating point numbers (i.e. 64 bits).
    - `float`s are value types in LPC, meaning they are copied when assigned,
      or passed to functions.
    - Examples:
        - `float x = 3.14;`
        - `float y = x;`
        - `float z = x + y;`

* `string`
    - A `string` is a sequence of UTF-8 encoded characters.
    - `string`s can be concatenated with the `+` operator, which will create a new `string`
      with the contents of both strings.
    - Two literal `string`s next to each other will be concatenated, e.g. `"hello" "world"` is
      equivalent to `"hello" + "world"`.
    - `string`s can be indexed with the `[]` operator, which will return the character at the
      given index. Indexing starts at 0.
    - `string`s can be repeated with the `*` operator, which will create a new `string` with
      the contents of the original repeated the given number of times.
    - `string`s are mutable, and are _value_ types in LPC, meaning they are
      copied when assigned, or passed to functions. This is a difference from
      numerous other languages.
    - Examples:
      ```c
      string x = "hello";
      string y = x;
      string z = x + y;
      string hi = "hello" + ", " + "world!";
      string hi2 = "hello" ", " "world!";
      string foos = "foo" * 3; // "foofoofoo"
      int i = x[0]; // 104
      x[1] = 'u'; // x becomes "hullo"
      ```

* `object`
    - An `object` is a reference to an instance of a type.
    - In LPC, an object corresponds to code in a single file, and includes the variables
        and functions defined in that file, along with any files it `inherit`s from.
    - `object` is a single type that encompasses all possible objects.
       Any further differentiation is left to the mudlib.
    - `object`s can either be a reference to a prototype object, sometimes called the master, 
      or a reference to a copy of the prototype, called a clone.
    - Masters are created by the VM when `clone_object` is called on an object where
      its prototype object has not been loaded, or a string is used as the receiver 
      for `call_other` (e.g. `"/secure/mail_daemon"->send_all()`)
    - New clones are created with the efun `clone_object`. This is the only way to create them.
    - _Existing_ clones can be found with `find_object`, by using their path, followed by a `#` and
      the clone number. For example, if you have a clone of `/std/goblin` with clone number
      3, you can find it with `find_object("/std/goblin#3")`. This same string syntax
      can be used in the `call_other` efun, as well.
    - It is a reference type in LPC, meaning it is a pointer to the actual instance, and
      changes to the instance will be reflected in all references to it.
    - Examples:
        ```c
        object gobbo = find_object("/std/goblin");
        object gobbo2 = clone_object("/std/goblin");
        object gobbo3 = gobbo2;
        gobbo3->set_name("gobbo3");
        gobbo2->query_name(); // "gobbo3"
        ```

* `mapping`
    - A `mapping` is a key-value store, where the keys and values can be any LPC type.
    - `mapping`s can be concatenated with the `+` operator, which will create a new `mapping`
      with the keys and values from both mappings.
    - `mapping`s are reference types in LPC, meaning it is a pointer to the actual container, and
      changes will be reflected in all references to it.
    - Examples:
        ```c
        mapping m = ([ "foo": 1, "bar": 2 ]);
        m["baz"] = 3;

        m["foo"]; // 1
        m["bar"]; // 2
        m["baz"]; // 3
        m["quux"]; // 0

        m_delkey(m, "bar") // 2;
        m["bar"]; // 0
        ```

* `function`
    - A `function` is a reference to a function, which can be called with the standard `( <args> )` syntax.
    - `function`s can be a reference to an efun, a static function written in LPC, a function in
      another object, or a closure.
      All variants can be passed where a `function` is expected, and all can be called with the standard syntax.
    - `function`s are reference types in LPC, meaning it is a pointer to the actual data, and
      changes will be reflected in all references to it.
    - There's a bunch to talk about with functions, so please see `function_type.md` for more details.
    - Examples:
        ```c
        function d = dump; // reference to an efun
        d("hello", "world!");
      
        function inc = &add(,1); // partial applications
        inc(5); // 6
      
        function e = &(some_ob)->greet(); // pointer to function in known object
        e(this_object());
      
        function f = &->greet(); // pointer to function in unknown object
        f(some_ob, this_object()); // first argument is the object to call the function on
        
        function g = (: [object pal] some_ob->greet(pal) :); // closure
        g(this_object());
        ```

* `mixed`
    - A `mixed` is a type that can be any other LPC type. It doesn't _change_ the underlying
      type of the data - it simply disables compile-time type checks.
    - Run-time checks can still fail.
    - Whether the value is a reference or value type depends on the underlying type.
    - Examples:
        ```c
        mixed x = 5;
        mixed y = "hello";
        mixed z = ({ 1, 2, 3 });
        mixed w = ([ "foo": 1, "bar": 2 ]);
        mixed v = &dump;
        mixed u = (: [object pal] some_ob->greet(pal) :);
        ```
      
* `void`
    - A `void` is a type that always evaluates to `0` if necessary, and is used 
      to indicate the absence of a value.
    - It is used as the return type of functions that don't return a value.
    - Closures always return a value, and will return `0` when returning the result of a `void` expression.
    - Variables _cannot_ be declared as `void`.
    - Examples:
        ```c
        void foo() {
            // do stuff
        }
        ```

* `<type> *` (array)
    - Arrays are a sequence of values of the same type.
    - Arrays can be created with the `({ <values> })` syntax.
    - Arrays can be indexed with numeric or range indices.
    - Arrays can be concatenated with the `+` operator.
    - `mixed *` indicates an array, where the values can be of any type, including other arrays.
    - `mixed *` is the only way to create a multidimensional array.
    - Arrays are mutable, and are reference types in LPC, meaning it is a pointer to the actual container, and
      changes will be reflected in all references to it.
    - Examples:
        ```c
        int *x = ({ 1, 2, 3 });
        string *y = ({ "foo", "bar", "baz" });
        mixed *z = ({ 1, "foo", ({ 1, 2, 3 }) });
        ```