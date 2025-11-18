use std.prelude.*;
const T = import "./import-test.ks";
const f = (arg :: T) => @comptime (
    module:
    let s = "Hello, World!";
);
const foo = f ();
use foo.*;
std.io.print s;
let x = 123;
@syntax inline_fn 100 wrap always = "fn" " " name "(" arg:any ")" " " "(" "\n\t" body:any ""/"\\\n" ")";
impl syntax (
    fn name(arg) (
        body
    )
) = `(
    let $name :: _ = $arg => $body
);
# impl syntax "inline_fn" = (.name, .arg, .body) => `(
#   let \name = \arg => \body
# );
fn f(x, .a, .b) (
    print x;
    print a;
    print (std.int32_to_string b);
);
f ("hi", .a = "a", .b = 5);
let ast = `(2 + 2);
`(2 + $ast)
