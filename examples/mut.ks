use std.*;

let mut x = "hello";
let f = () => print x;
f();
(
    # testing mutating from nested scope
    x = "world";
);
f();
