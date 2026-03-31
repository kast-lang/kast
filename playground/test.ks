let f = () => print("!@#!#!@");
let a = { .field = "Hello", .other_field = f, .opt = :Some (1 :: Int32) };
let b = { .field = "Hello", .other_field = f, .opt = :Some (1 :: Int32) };
dbg.print(std.repr.physically_equal(a, b));
dbg.print(std.repr.structurally_equal(a, b));

