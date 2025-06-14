use std.*;
const loop_handler_type = forall[T] { T -> () with output };
let loop_handler = c => dbg c;
const yielded_type = _;
comptime dbg yielded_type;
with (loop_handler :: loop_handler_type[yielded_type]);
comptime dbg yielded_type;

(
    (current loop_handler_type[char])('c');
    comptime print "here";
) ::with loop_handler_type[yielded_type];
comptime print "the end";
