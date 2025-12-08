use std.prelude.*;
use std.collections.treap;
let v = treap.create ();
for i in 0..10 do (
    v = treap.merge (v, treap.singleton (i + 10));
);
std.dbg.print v;
treap.iter (&v, &x => dbg.print x);
let left, right = treap.split_at (v, 8);
print "left:";
treap.iter (&left, &x => dbg.print x);
print "right:";
treap.iter (&right, &x => dbg.print x);
print "====";
std.dbg.print <| (treap.at (&v, 5))^;
print "====";
let v = treap.set_at (v, 7, 67);
treap.iter (&v, &x => dbg.print x);
