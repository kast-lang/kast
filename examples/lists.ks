use std.prelude.*;
let a = list.create ();
a = list.add (a, 1);
a = list.add (a, 2);
a = list.add (a, 3);
list.iter (a, dbg.print[_]);
