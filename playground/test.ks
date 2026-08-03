module:

const std = include "../examples/stripped-std.ks";

use std.*;

const g = [T] (x :: T) => (
    print_String("Im a G\n");
);
const f = [T] (x :: T) => (
    g[T](x);
);

f[Int](123);
print_String("Hello\n");
