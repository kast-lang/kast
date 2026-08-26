let foo = [X] (a :: X, b :: X) => std.cmp.less[X](a, b);

let a :: Int32 = 1;
let b = 2;

dbg.print(foo(a, b));
