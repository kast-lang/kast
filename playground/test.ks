use std.rc.Rc;

let tuple = { .named = Rc.new(420) };
let _ = { ...tuple, ...tuple };
