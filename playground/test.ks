use std.rc.Rc;

let mut a = ArrayList.new();
# ...

let ref_to_el_0 = &a.[0];

&mut a |> ArrayList.push_back(123);

