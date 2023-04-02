# call_other

`mixed call_other(object ob|object *obs|mapping map|string path|string *paths, string name, ...)`

#### Alternate Syntax (preferred)
* `ob->name(...)`
* `({ <array items> })->name(...)`
* `([ <mapping items> ])->name(...)`
* etc...

Call a function contained within another object, or a collection of objects.
The first argument determines the object(s) to call the function on, and the
shape of the return value:

* Single object - calls on that object, and returns the result
* Array of objects - calls on all objects in the array, and 
  returns an array with all the results
* Mapping - calls on all values in the mapping, and returns a new mapping with 
  the keys of the original, and the results as the values
* String - calls on the object with that file name (see `file_name` docs), including
  clone ID, and returns the result
* Array of strings - calls on all objects with the file names in the array, and 
  returns an array with all the results

This function is the only way to call functions in other objects.

### Examples

```c
object ob = clone_object("/lib/test/test_object");
int i = ob->add_two(4, 6);
int j = call_other(ob, "add_two", 4, 6);

dump(i == j); // 1 (true)


object *foo = ({
   clone_object("/lib/test/test_object"),
   clone_object("/lib/test/test_object"),
});

int *results = call_other(foo, "add_two", 4, 6);
```
