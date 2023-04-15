TODO: finish this

```c
inherit "/std/object";
inherit "/lib/container";

string name; // public - directly accessible by inheritors
protected float shoe_size; // protected - directly accessible by inheritors
private int volume // private - only accessible by this object. Completely invisible to inheritors.

void create() {
    ::create();
    set_name("foo");
    shoe_size = 12.5;
    volume = 9001;
}

private void set_name(string n) {
    name = n;
}
```

notes:

- no prototypes necessary
- decls can be anywhere in the body (globals are hoisted for codegen, but not for semantic analysis)
- globals are 'global' only to the program, and cannot be accessed from other programs. they just outlive function invocations.
- globals are initialized when the object is created, prior to calling `create`, (which happens automatically)