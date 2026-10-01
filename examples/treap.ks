use std.collections.Treap;
let mut v = Treap.new();
for i in 0..10 do (
    v = Treap.join(v, Treap.singleton(i + 10));
);
# std.dbg.print v;
let Treap_to_string = v => Treap.to_string(v, &x => to_string(x));
print(&@interpolate_String("v = ", Treap_to_string(&v)) |> String.as_str);
let { left, right } = Treap.split_at(v, 8);
print(&@interpolate_String(
    "split_at 8:\n  left = ",
    Treap_to_string(&left),
    "\n  right = ",
    Treap_to_string(&right),
) |> String.as_str);
let v = Treap.join(left, right);
print(&@interpolate_String(
    "at 5 = ",
    to_string((Treap.at(&v, 5))^),
) |> String.as_str);
let v = Treap.set_at(v, 7, 67);
print("set at 7 = 67");
print(&@interpolate_String(
    "v = ",
    Treap_to_string(&v),
) |> String.as_str);
