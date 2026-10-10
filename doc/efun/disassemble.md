# disassemble

`string *disassemble(object ob)`

Return a fresh array of diagnostic lines describing the live object's current
compiled program. The listing includes inherited functions, private and protected
definitions, generated closures and initializers. It renders function headers,
constant pools, runtime types, argument lists, labels and instructions, with
inherited global operands projected into the receiving object's layout.

The program comes from the calling transaction's snapshot, including any
recompilation visible to that transaction. Clones use their current compiled
program. The listing contains compiled constants, including string literals;
it does not read runtime global values or captured closure values. Inspection
does not grant permission to call private or protected functions.

The argument is required and must be a live object. Other values, including
`nil`, `0`, path strings and destructed objects, raise a runtime error. Programs
without user-defined functions can still list generated initializers.
No authorization apply is called. The efun does not load objects, invoke functions,
initialize the target or write files.

Each line is a separate string without a trailing newline. The complete listing
may exceed the 8,192-byte string limit; an individual line exceeding that limit
raises an error rather than truncating the listing. Modifying the returned array
does not change the program or future listings. The format is for diagnostics
and may change with compiler or instruction-set changes; it is not a serialized
program or a stable machine-readable interface.

```c
foreach (string line in disassemble(this_object())) {
    write(line);
    write("\n");
}
```

The CLI's [`-S` / `--emit-asm`](../../lpc-rs-lpcc/README.md#usage) prints the same
instruction format while compiling source without executing LPC initializers.

## See also

[`compile_string`](compile_string.md), [`functions`](functions.md),
[`function_info`](function_info.md), [`variable_info`](variable_info.md)
