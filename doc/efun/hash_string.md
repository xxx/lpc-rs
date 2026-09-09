# hash_string

`string hash_string(string|int str, int algorithm)`

Returns the lowercase hexadecimal digest of `str`, using CD's algorithm IDs:

| Algorithm | ID | Hexadecimal characters |
|---|---|---|
| MD5 | `0` | 32 |
| SHA-1 | `1` | 40 |
| SHA-256 | `2` | 64 |
| SHA-384 | `3` | 96 |
| SHA-512 | `4` | 128 |

Both arguments are required. An integer `str` returns `0` without inspecting
the algorithm, as in CD. For string input, an unknown algorithm or a
non-integer algorithm is a runtime error; other input types are also errors.
Strings are hashed as UTF-8 bytes up to the first NUL, matching CD's
`hash_string`. No salt or algorithm prefix is added, and repeated calls
with the same input and algorithm return the same digest.

```c
hash_string("foobar", 1);
// "8843d7f92416211de9ebb963ff4ce28125932878"

if (hash_string(typed, HASH_STRING_SHA512) == stored)
    write("Password matches.\n");
```

The `HASH_STRING_*` constants belong to the mudlib and must have the numeric
values above. `hash_string` supports checksums and verification of legacy
password records; use `crypt` for newly stored passwords.

### See also

`crypt`
