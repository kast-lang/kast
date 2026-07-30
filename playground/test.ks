module:

const std = include "../examples/stripped-std.ks";

use std.*;

const g = [T] () => (
    print_String("Im a G\n");
);
const f = [T] () => (
    g[T]();
);

f[Int]();
print_String("Hello\n");