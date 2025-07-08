use std.prelude.*;
const f = () => comptime (
  module:
  let s = "Hello, World!";
);
const foo = f ();
use foo.*;
std.io.print s;
let x = 123;
syntax inline_fn 100 wrap never = "fn" " " name " " arg " " "=>" " " body;
impl syntax (fn name arg => body) = `(
  let \name = \arg => \body
);
# impl syntax "inline_fn" = (.name, .arg, .body) => `(
#   let \name = \arg => \body
# );
fn f (x, .a, .b) => (
  print x;
  print a;
  print (std.int32_to_string b);
);
let ast = `(2 + 2);
`(2 + \ast)
