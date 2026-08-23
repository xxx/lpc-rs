// Created on first call by the concurrent-create test; every caller must
// end up talking to this one object.
object who() {
    return this_object();
}
