use std.prelude.*;
const f = () => comptime (
  module:
  let s = "Hello, World!";
);
const foo = f ();
use foo.*;
std.io.print s;
let x = 123;
let f = (x, .a, .b) => (
  print x;
  print a;
  print (std.int32_to_string b);
);
