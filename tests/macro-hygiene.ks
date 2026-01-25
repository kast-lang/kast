#!/usr/bin/env -S kast run --no-std
# this is part of std
const Ast = @native "ast";
impl syntax (macro!(arg)) = `(
    include_ast $macro(`($arg))
);
const dbg = {
    .print = [T] (x :: T) -> () => (
        (@native "dbg.print")(x)
    ),
};
# actual code
(
    const macro = (inner :: Ast) -> Ast => `(
        let _ :: Ast = _;
        let x = "x_in_macro_def";
        dbg.print({ x, $inner });
    );
    let x = "x_global";
    macro!(x);
);

@syntax "custom_let" 10 @wrap never = "custom_let" " " pattern " " "=" " " value;
impl syntax (custom_let pattern = value) = `(
    let $pattern = $value
);

impl syntax (for pattern in value do body) = `(
    let loop_body = (value) => (
        # custom_let $pattern = value;
        let $pattern = value;
        let x = 123;
        $body;
    );
    let value = $value;
    loop_body(value.x);
    loop_body(value.y);
);

for x in { .x = "hello", .y = "world" } do (
    (@native "dbg.print")(x);
);
# x_scope: (
#   let x = _;
# )
# let loop_body = (value) => (
#     let (x) = value;
#     let x = 123;
#     (@native "dbg.print")(x);
# );
# let value = (.x = "hello", .y = "world");
# loop_body(value.x);
# loop_body(value.y);
