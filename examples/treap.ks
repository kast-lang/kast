use std.prelude.*;
use std.collections.treap;
let v = treap.merge (
    treap.merge (
        treap.singleton 1,
        treap.singleton 2,
    ),
    treap.singleton 3,
);
std.dbg.print v;
treap.iter (v, std.dbg.print[_]);
