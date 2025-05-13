use std.*;

let x;

let mut x :: int32 = 123;
x += 1;

let foo = (.a = &x, .b = "hi");
dbg foo;

let mut a = list[1, 2, 3] :: list[int32];
dbg &a;
# TODO this should not compile?
let elem1 = list_get (&a, 1);
let elem2 = list_get (&a, 2);
dbg elem1;
elem1^ = 5;
dbg &a;
