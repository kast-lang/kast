use std.*;

const mod = struct (
    const Foo = newtype ( .x = int64 );
    const m = macro (body) => (
        let ty :: type = eval_ast `(typeof $body);
        if ty == int32 then (
            `(dbg ( ( .x = 123 ) :: Foo ))
        ) else
            `(print "not an int32!")
    );
);

let x :: int32;
mod.m!(x);
