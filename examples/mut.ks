use std;

let mut x = "hello";
let f = () => print x;
f();
x = "world";
f();
