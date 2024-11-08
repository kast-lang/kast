# TODO (~a, ~b)
const call_both = macro (a: a, b: b) => `($a(); $b());

let hello = () => std.print "hello";
let world = () => std.print "world";

(
    syntax call_both <- 20 = "call" a "and" b;
    call hello and world;
);

(
    syntax call_both <- 20 = "call" b "and" a;
    call hello and world
);
