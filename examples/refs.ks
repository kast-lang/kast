use std.prelude.*;

let mut foo = (.x = 123);
foo.x = 1;
(&mut foo.x)^ = 2;
dbg.print(foo);
let x = &mut foo.x;
x^ = 3;
dbg.print(foo);
