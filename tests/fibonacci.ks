module:

const Int = Int32;

const fib = (n :: Int) -> Int => (
    if n < 2 then (
        1
    ) else (
        fib(n - 1) + fib(n - 2)
    )
);

dbg.print(fib(10));
