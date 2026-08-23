int parent_val = 100;
function parent_closure() { int p = 1; return (: p + parent_val :); }
int parent_direct() { int p = 1; function f = (: p + parent_val :); return f(); }
