# hash_string

`string hash_string(string|int str, int algorithm)`

Returns the lowercase hexadecimal digest of `str`, using the selected algorithm:

| Algorithm | ID | Hexadecimal characters |
|---|---|---|
| MD5 | `0` | 32 |
| SHA-1 | `1` | 40 |
| SHA-256 | `2` | 64 |
| SHA-384 | `3` | 96 |
| SHA-512 | `4` | 128 |

Both arguments are required. An integer `str` returns `0` without inspecting
the algorithm. For string input, an unknown or non-integer algorithm is a
runtime error; other input types are also errors.
Strings are hashed as UTF-8 bytes up to the first NUL. No salt or algorithm
prefix is added, and repeated calls with the same input and algorithm return
the same digest.

```c
hash_string("foobar", 1);
// "8843d7f92416211de9ebb963ff4ce28125932878"
```

Use `hash_string` for checksums and `crypt` for password hashing.

### See also

`crypt`
