inherit "./grandparent";

public int b = 98765;

private int priv = 554;

void create() {
    dump("parent create!");
}

void parent_method() {
    dump("parent method!");
}

void overridden_method() {
    dump("parent overridden method!");
}

mixed * get_vars() {
    return ({ a, b, priv });
}
