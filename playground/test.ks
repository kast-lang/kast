const foo = (.a :: Int32, .b :: Int32) => a + b;

let result = foo(.b = 2, .a = 3);
dbg.print(result);
