module:

const std = include "../examples/stripped-std.ks";

use std.*;
with PanicHandler = default_panic_handler;

print_Int(2 + 2);
print_String("\n");

(#
const break_out_of_block = (token, value) => (
    print_String("unwind\n");
    unwind token value;
    print_String("after unwind\n");
);

let result :: Int = unwindable block (
    print_String("begin unwindable block\n");
    break_out_of_block(block, 12345);
    print_String("endof unwindable block\n");
    456
);

print_String("result of unwindable = ");
print_Int(result);
print_String("\n");
#)


let x = &mut 5;
x^ = 2;
print_Int(5);

