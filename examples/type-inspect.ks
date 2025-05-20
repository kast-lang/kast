use std.*;

const m = macro (body) => (
    let ty :: type = eval_ast body;
    if ty == int32 then
        `(print "had an int32!")
    else
        `(print "not an int32!")
);

const x :: (type, type) = (_, string);
let _ :: x.0 = 123 :: int32;
comptime dbg x.0;
m!(x.0);
