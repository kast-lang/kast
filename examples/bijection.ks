use std.*;

comptime with default_number_type_based_on_dot;

syntax bijection <- 10 = lhs "<=>" rhs;
impl syntax bijection = macro (.lhs, .rhs) => `(
    .into = fn ($lhs) with () { $rhs },
    .from = fn ($rhs) with () { $lhs },
);

let foo = (x, y) <=> (y, x);
let a = 123, "hello";
dbg &a;
let b = foo.into a;
dbg &b;
let a-again = foo.from b;
dbg &a-again;
