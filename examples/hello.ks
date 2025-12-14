use std.prelude.*;

const Foo = type (.x :: int32, .y :: int32);
let foo :: Foo = (.x = 1, .y = 2);
let foo2 :: Foo = (...foo, .x = 3);
dbg.print foo2;
