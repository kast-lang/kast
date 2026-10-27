use std.rc.Rc;

const f = (...) => ();

let { first, ...rest } = { Rc.new(69), Rc.new(420) };

f(Rc.new(123 :: Int32));
