use std.collections.Treap;

let mut t = Treap.new();
for i in 0..100000 do (
    t = Treap.join(t, Treap.singleton(i));
);

