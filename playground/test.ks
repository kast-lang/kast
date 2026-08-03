module:

const std = include "../examples/stripped-std.ks";

use std.*;
with PanicHandler = default_panic_handler;

print_Int(2 + 2);
print_String("\n");

