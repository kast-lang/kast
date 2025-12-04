use std.prelude.*;

let foo = ( .x = 123 );
foo.x = 1;
(&foo.x)^ = 2;
dbg.print foo;
let x = &foo.x;
x^ = 3;
dbg.print foo;
