# Comparisons

`<`, `<=`, `>`, `>=`, `==` and `!=` return integer `1` for true and `0` for false.

## Numbers

Integers and floats can be compared in either order, without a warning. The
comparison uses their numeric values without first rounding the integer to a
float. It does not change either operand's type.

```c
1 == 1.0;                             // 1
1 < 1.5;                              // 1
-1 > -1.5;                            // 1
9007199254740993 > 9007199254740992.0;  // 1
9007199254740993 == 9007199254740992.0; // 0
```

This preserves the distinction between large integers even where a float cannot
represent every integer. It cannot recover precision already lost in a float
literal, conversion or calculation. Mixed arithmetic still promotes the integer
to float, and assignments, returns, arguments and casts retain their type rules.
Use `to_int` or `to_float` when a conversion is required.

Positive and negative floating zero compare equal to integer zero. Float
ordering places negative infinity below finite numbers, positive infinity above
them, and NaN last; all NaN representations compare equal to each other.

[`member_array`](../efun/member_array.md) uses the same equality. Natural
[`sort_array`](../efun/sort_array.md), [`min`](../efun/min.md) and
[`max`](../efun/max.md) use the same exact numeric ordering. Equal values retain
their input order in a natural sort, and the first equal extreme wins in
`min` and `max`; values keep their original types.

## Other values

Strings and bytes compare by content within their own type. Arrays, mappings,
classes, objects and function values compare equal by identity. Other pairs of
different types are unequal, subject to [null and destructed-object
behavior](null.md). Mixing bytes and strings is an error.

Ordering requires numbers, two strings or two byte sequences. Statically known
incompatible operands are rejected; if reached through `mixed`, an unordered
pair makes all four ordering operators false. Mixing bytes and strings still
raises an error at runtime.

In zero mode, `nil` is integer zero, so `nil == 0.0` is true. In distinct mode it
is false. Float zero is never a null value for type checks or `nilp`.

## Collection matching

Mapping keys, array set operations (`&`, `|`, `-`), and `unique_array` grouping
and skip keys use type-sensitive matching. Integer `1` and float `1.0` remain
separate keys and members even though `1 == 1.0` is true.

```c
mapping m = ([ 1: "integer", 1.0: "float" ]);
sizeof(m);                         // 2
member_array(1, ({ 1.0 }));         // 0
({ 1 }) & ({ 1.0 });               // ({ })
```
