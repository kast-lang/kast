use std.collections.Treap;
let mut v :: Treap.t[Int32] = Treap.new();
for i in 0..10 do (
    v = Treap.join(v, Treap.singleton(i + 10));
);
# std.dbg.print v;
let Treap_to_string = v => Treap.to_string(v, &x => to_string(x));
println!("v = \(Treap_to_string(&v))");
let { left, right } = Treap.split_at(v, 8);
println!(''
    split_at 8:
        left = \(Treap_to_string(&left))
        right = \(Treap_to_string(&right))'');
let v = Treap.join(left, right);
println!("at 5 = \(Treap.at(&v, 5)^)");
let v = Treap.set_at(v, 7, 67);
println!("set at 7 = 67");
println!("v = \(Treap_to_string(&v))");

