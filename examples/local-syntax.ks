const call-both = macro (.a, .b) => `($a(); $b());

let hello = () => std.print "hello";
let world = () => std.print "world";

(
    syntax call-1-then-2 <- 20 = "call" a "and" b;
    impl syntax call-1-then-2 = call-both;
    call hello and world;
);

(
    syntax call-2-then-1 <- 20 = "call" b "and" a;
    impl syntax call-2-then-1 = call-both;
    call hello and world
);
