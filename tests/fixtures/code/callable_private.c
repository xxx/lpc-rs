private mixed token = "private data";
private function hidden = (: 123 :);
private mixed hidden_mixed = (: 456 :);
private function shared = (: 999 :);

int token(int n = 0) { return 42 + n; }
int parent_private() { return hidden() + hidden_mixed(); }
int parent_partial() { return evaluate(&hidden()); }
function parent_closure() { return (: hidden() + hidden_mixed() :); }
