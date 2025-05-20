use std.*;

const m = macro (body) => (
    let ty :: type = eval_ast `(typeof $body);
    if ty == int32 then
        `(print "had an int32!")
    else
        `(print "not an int32!")
);

let x :: int32;
m!(x);
