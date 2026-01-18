const double = (ast) => `(
    let x = 123;
    $ast + $ast
);

let x = 2;
dbg.print(double!(x));
