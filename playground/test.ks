module:

const std = include "../examples/stripped-std.ks";

use std.*;
with PanicHandler = default_panic_handler;

const g = [T] (x :: T) => (
    print_String("Im a G\n");
);
const f = [T] (x :: T) => (
    g[T](x);
);

f[Int](123);
print_String("Hello\n");
let x :: Int = panic("I panicked") |> from_never;
