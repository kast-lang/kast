let call_both = macro args => `($args.a(); $args.b());

let hello = _ => print "hello";
let world = _ => print "world";

(
    syntax call_both <- 20 = "call" a "and" b;
    call hello and world;
);

(
    syntax call_both <- 20 = "call" b "and" a;
    call hello and world
);