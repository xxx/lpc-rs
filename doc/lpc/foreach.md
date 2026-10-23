# foreach

`foreach` accepts either `in` or `:` between its loop variables and the collection:

```c
foreach (int value in numbers) {
    total += value;
}

foreach (int value : numbers) {
    total += value;
}
```

The two spellings have identical behavior. Use one variable for an array, string,
or `bytes` value. String iteration yields integer Unicode codepoints; byte
iteration yields integers from 0 to 255. Use two variables for mapping keys and
values:

```c
foreach (string name, int score in scores) {
    total += score;
}
```

Loop variables are declared in the loop's scope. Their types can be omitted,
defaulting to `mixed`. Array types and visible [class types](classes.md) can also
be used. Existing type checks apply with either separator.

The collection expression is evaluated once when the loop starts. Reassigning
the source variable inside the body does not change which collection the loop
traverses:

```c
string *items = ({ "skip", "keep" });
foreach (string item in items) {
    if (item == "skip") items -= ({ item });
}
// Both original elements were visited; items now contains only "keep".
```

The loop retains the collection value, without copying its elements. Arrays and
mappings remain shared: updates to their existing elements can affect later
iterations. The length is recorded at loop entry; changing the size of the
retained collection does not update that bound and can cause an indexing error.

An empty collection skips the body. The body can be a single statement or a block;
`continue` advances to the next iteration, and `break` exits the loop.

`in` is recognized only in the separator position. It remains available as a
variable, function, or field name elsewhere, including as the loop variable:

```c
foreach (int in in numbers) {
    total += in;
}
```

Macros expand before the separator is recognized. Other words, such as `from`,
are rejected there. `in` is not a general expression operator.
