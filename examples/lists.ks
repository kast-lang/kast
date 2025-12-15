use std.prelude.*;
let a = List.create ();
List.push_back (&a, 1);
List.push_back (&a, 2);
List.push_back (&a, 3);
List.iter (&a, &x => dbg.print x);
