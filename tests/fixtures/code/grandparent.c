int a = 123;
public int b = 456;

private int priv = 666;

void create() {
    dump("grandparent create!");
}

void grandparent_method() {
    dump("grandparent method!");
}

//nomask void nomask_method() {
void nomask_method() {
    dump("grandparent nomask method");
}

void overridden_method() {
    dump("grandparent overridden method!");
}

private mixed * get_vars() {
    return ({ a, b, priv });
}
