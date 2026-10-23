# Array literal spreading

Inside an array literal, `xs...` inserts each element of `xs` in order:

```c
int *xs = ({ 2, 3 });
int *ys = ({ 1, xs..., 4 });  // ({ 1, 2, 3, 4 })
int *copy = ({ xs... });     // a new array containing 2 and 3
```

A literal can contain multiple spreads, in any position. An empty array adds
no elements. Spreads also accept expressions, including calls, slices and other
array literals:

```c
int *xs = ({ 1, 3, 4 });
int *ys = ({ xs[0..0]..., 2, xs[1..]..., 5 });
// ys is ({ 1, 2, 3, 4, 5 })
```

Expansion copies one level into a fresh array. Nested arrays, mappings, class
instances and objects retain their shared identities. Without `...`, an array
expression inserts the array itself as one element. Use
[`deep_copy`](../efun/deep_copy.md) when nested mutable values need copying too.

## Types and errors

The operand must have an array type or `mixed`. A statically known scalar,
including a string, is a compile error. At runtime every spread must contain an
array; a non-array value held in `mixed` raises an error. Neither `0` nor `nil`
stands for an empty array.

Literal type inference uses the spread's element type: `({ ints... })` is
`int *` when `ints` is `int *`. Combining different element types produces
`mixed *`, following the usual array literal rules. A `mixed` operand does not
establish a more specific element type.

## Evaluation order

Element expressions run once each, from left to right. After all expressions
have run, their saved values are expanded into the result. Reassigning an earlier
operand's variable does not replace that saved value. Mutating the contents of
its array does affect the expansion:

```c
int *xs = ({ 1 });
int *ys = ({ xs..., xs[0] = 2 });  // ({ 2, 2 })
```

The expanded length is not subject to a function call's argument-count limit.
`...` is also available for [argument spreading](argument_spreading.md); it is
not a standalone operator or mapping-entry syntax.
