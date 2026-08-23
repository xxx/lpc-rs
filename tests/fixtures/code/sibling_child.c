inherit "/sibling_a";
inherit "/sibling_b";
int c_seq = na + nb;
string created_by;
int creates;
void create() { created_by = "child"; creates++; }
mixed snapshot() {
    function f1 = fa;
    function f2 = fb;
    return ({ sa, sb, f1(), f2(), na, nb, c_seq, created_by, creates, a_create, b_create, one(), two() });
}
