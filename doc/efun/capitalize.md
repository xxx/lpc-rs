# capitalize

`string capitalize(string str)`

Return `str` with its first character converted to upper case and the rest
unchanged. Case mapping is Unicode, not ASCII only. A first character with
no upper-case form, such as a digit, is left as it is.

### Examples

```c
capitalize("hello world");   // "Hello world"
```

### See also

`lower_case`, `upper_case`
