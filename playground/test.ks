module:

const std = include "../examples/stripped-std.ks";

use std.*;
with PanicHandler = default_panic_handler;

let m = (
    module:
    
    let x = 123;
    let f = (arg :: Int) => (
        () => print_Int(x)
    );

    f(0)();
);