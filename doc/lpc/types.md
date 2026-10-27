# lpc-rs supported types

Named [classes](classes.md) support shared instances, named construction,
guarded member access, shallow copying with `copy_class()`, and native persistence.
[Null values and `nil`](null.md) describes the configurable null representation;
examples on this page use the default `NULL_MODE=zero` unless stated otherwise.
[Comparisons](comparisons.md) describes exact int/float comparison and the
type-sensitive matching used by mapping keys and array set operations.

* `int`
  - Run-of-the-mill signed integers, 64 bits by default. This size is ostensibly
    changeable in `lpc-rs-core`, but no testing is done on different sizes. 
  - Integer arithmetic wraps at the integer width, including folded literals.
    Dividing the minimum integer by `-1` returns that minimum; its remainder is `0`.
    Division and remainder by zero raise runtime errors, including literal expressions.
  - Shift counts are reduced modulo the integer width, including negative counts.
    With 64-bit integers, `1 << 64` is `1`, and `1 << -1` equals `1 << 63`.
  - In the default zero mode, `0` doubles as null and fits any value type.
    Distinct mode uses `nil` for absence and treats `0` as an ordinary integer.
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
    - A `string` literal decodes these escapes; any other `\x` is just `x`.

      | Escape | Character       | Escape | Character       |
      |--------|-----------------|--------|-----------------|
      | `\n`   | newline         | `\b`   | backspace       |
      | `\r`   | carriage return | `\a`   | bell            |
      | `\t`   | tab             | `\"`   | `"`             |
      | `\v`   | vertical tab    | `\\`   | `\`             |
      | `\f`   | form feed       | `\'`   | `'`             |

    - `string`s can be indexed with the `[]` operator: `s[i]` is the character code (an
      `int`) at index `i`, and `s[a..b]` is the substring from `a` to `b` inclusive (a
      `string`). Indexing starts at 0; negative indices and explicit `<n` indices
      count from the end. See [indexing and slices](indexing.md) for bounds and writes.
    - `string`s can be repeated with the `*` operator and an integer count, in
      either operand order. Zero and negative counts produce an empty string.
      The result may not exceed 8,192 UTF-8 bytes; exceeding this limit raises
      an error. Oversized constant expressions can be rejected during
      compilation; dynamic overflow raises a catchable runtime error.
    - `string`s are value types in LPC. Assigning a new string does not change
      another variable holding the old value. Plain element and slice assignment
      construct a new string and rebind its owner; see [string writes](indexing.md#string-element-assignment).
    - Examples:
      ```c
      string x = "hello";
      string y = x;
      string z = x + y;
      string hi = "hello" + ", " + "world!";
      string hi2 = "hello" ", " "world!";
      string foos = "foo" * 3; // "foofoofoo"
      int i = x[0]; // 104
      x = x[0..0] + "u" + x[2..]; // x becomes "hullo"
      ```

* `bytes`
    - A `bytes` is an immutable sequence of bytes.
    - There is no `bytes` literal. A value comes from `read_bytes`, `to_bytes(string, encoding)`,
      `to_bytes(int *)`, the `+` operator, or slicing an existing `bytes`.
    - `bytes` values can be concatenated with the `+` operator, which will create a new `bytes`
      with the contents of both, capped the same way `string` concatenation is. A `bytes` and a
      `string` never mix: `+`, `==`, `!=`, `<` and the other comparisons between them, `to_string`
      of a `bytes`, and `%s` of a `bytes` are all runtime errors naming `to_text` or `to_bytes` as
      the conversion to make instead.
    - `bytes` can be indexed with the `[]` operator: `b[i]` is the byte value (an `int`, 0 to 255)
      at index `i` -- 0 when `i` falls outside the buffer -- and `b[a..c]` is the subsequence from
      `a` to `c` inclusive (a `bytes`). Indexing starts at 0; negative indices count from the end,
      as with `string`s. Explicit `<n` indices and slice endpoints are also supported;
      see [indexing and slices](indexing.md).
    - `sizeof(b)` is the number of bytes; `foreach (int c : b)` visits each byte in order.
    - `==`, `!=`, `<` and the other comparisons between two `bytes` values compare them by
      content, byte by byte.
    - `bytes` are immutable: `b[i] = x` is refused.
    - Printing (`dump`, `%O`, error messages) shows a `bytes` as `b"..."`: printable ASCII as
      itself, `"` and `\` escaped, every other byte as `\xNN`.
    - Examples:
      ```c
      bytes b = read_bytes("/d/map.m");
      bytes utf8 = to_bytes("hello", "UTF-8");
      int first = utf8[0];      // 104
      bytes tail = utf8[1..3];  // b"ell"
      bytes both = b + tail;
      ```

* `object`
    - An `object` is a reference to an instance of a type.
    - In LPC, an object corresponds to code in a single file, and includes the variables
        and functions defined in that file, along with any files it `inherit`s from.
    - `object` is a single type that encompasses all possible objects.
       Any further differentiation is left to the mudlib.
    - `object`s can either be a reference to a prototype object, sometimes called the master, 
      or a reference to a copy of the prototype, called a clone.
    - Masters are loaded by `load_object`, by `clone_object` when the prototype
      is not loaded, or by using a string receiver for `call_other`
      (e.g. `"/secure/mail_daemon"->send_all()`).
    - New clones are created with the efun `clone_object`. This is the only way to create them.
    - `object_clones(ob)` lists the live clones sharing `ob`'s compiled program.
    - [`objects()`](../efun/objects.md) lists all live loaded objects, including prototypes and clones.
    - _Existing_ clones can be found with `find_object`, by using their path, followed by a `#` and
      the clone number. For example, if you have a clone of `/std/goblin` with clone number
      3, you can find it with `find_object("/std/goblin#3")`. This same string syntax
      can be used in the `call_other` efun, as well.
    - It is a reference type in LPC, meaning it is a pointer to the actual instance, and
      changes to the instance will be reflected in all references to it.
    - Examples:
        ```c
        object gobbo = load_object("/std/goblin");
        object gobbo2 = clone_object("/std/goblin");
        object gobbo3 = gobbo2;
        gobbo3->set_name("gobbo3");
        gobbo2->query_name(); // "gobbo3"
        ```

* `mapping`
    - A `mapping` is a key-value store, where the keys and values can be any LPC type.
    - Mapping literals evaluate each key and value in source order, including entries
      whose keys appear more than once.
    - Repeated keys keep the last value and retain their first insertion position.
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

        function h = (: greet($(this_player())) :); // save the player at creation
        ```

    - [`$(expression)` captures a value at closure creation](function_type.md#explicit-value-captures);
      ordinary closure references continue sharing the original variables.

* `mixed`
    - A `mixed` is a type that can be any other LPC type. It doesn't _change_ the underlying
      type of the data - it simply disables compile-time type checks.
    - Run-time checks can still fail.
    - A `mixed` is taken wherever a value is checked: as either operand of an operator, as
      an index (`a[m]`), as the operand of `-`, `++`, `--` and `~`, and a slice of one
      (`m[1..2]`) is `mixed`, since the value may be a string. The runtime checks the
      value: `-m` on a string, `m++` on an array and `a[m]` with a string in `m` are
      runtime errors. A `mixed *` is an array, so `-ma`, `ma++` and `a[ma]` are compile
      errors. A slice is a value, so `m[1..2]++` is a compile error.
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
    - [Array literal spreading](array_spreading.md) inserts another array's elements: `({ 1, xs..., 2 })`.
    - Arrays can be indexed with numeric or range indices.
    - Arrays can be concatenated with the `+` operator.
    - `a & b` is the members of `a` also found in `b`, in `a`'s order, duplicates
      kept; `a - b` removes every occurrence of `b`'s members; `a | b` is `a` as
      it is, then `b`'s members absent from `a` in `b`'s order, duplicates included.
      Members are compared by content for strings and `bytes` and by identity for
      live objects, nested arrays, mappings, classes and functions. Destroyed
      objects match the configured null value on either side; numeric matching
      remains type-sensitive. Each operation makes a fresh outer array without
      changing the operands or the selected members' stored values. See
      [collection matching](comparisons.md#collection-matching).
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

* Casts
    - `(type) expr` asserts that the value is `type` or `0`: the expression's static type
      becomes `type`, and at runtime a value of another type is an error
      (`runtime error: cast to int of string`). Arrays take the `*` form: `(string *) list`
      checks that the value is an array, not its elements. A destructed object passes every
      cast, the same as `0`.
    - A cast converts nothing; an `int` is never a `float` through one. `to_int`, `to_float`
      and `to_string` convert.
    - A cast whose source can never be the target is a compile error: `(int) some_string`.
      `mixed`, an equal type, a union with the target among its members, and a literal `0`
      are the sources a cast accepts.
    - `(void)` is not a cast target; `(void) expr` is a compile error.
