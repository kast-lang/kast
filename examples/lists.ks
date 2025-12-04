use std.prelude.*;
let a = list.create ();
list.push_back (&a, 1);
list.push_back (&a, 2);
list.push_back (&a, 3);
list.iter (&a, x => dbg.print x^);
