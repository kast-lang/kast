module:

use std.*;

let fib = fn (n :: int32) -> int32 {
    if n < 2 then
        1
    else
        fib (n - 1) + fib (n - 2)
};
