use std.prelude.*;
use std.collections.treap;
let v = treap.create ();
for i in 0..10 do (
    v = treap.merge (v, treap.singleton (i + 10));
);
# std.dbg.print v;
let treap_to_string = v => treap.to_string (v, &x => to_string x);
print ("v = " + treap_to_string &v);
let left, right = treap.split_at (v, 8);
print ("split_at 8:\n  left = " + treap_to_string &left + "\n  right = " + treap_to_string &right);
print ("at 5 = " + to_string (treap.at (&v, 5))^);
let v = treap.set_at (v, 7, 67);
print ("set at 7 = 67");
print ("v = " + treap_to_string &v);
