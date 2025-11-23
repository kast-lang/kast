use std.prelude.*;
let a = list.create[_] ();
a=list.add[_] (a, 1);
a=list.add[_] (a, 2);
a=list.add[_] (a, 3);
dbg.print[_] a;
list.print[_] (dbg.print[_], a)
