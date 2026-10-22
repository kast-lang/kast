use std.rc.Rc;

let x = Rc.new(String.from_str("im refcounted"));
let y = x;
let z = y;

println!("x = \(x^), y = \(y^), z = \(z^)");
