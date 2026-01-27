use std.Ast;

@syntax test 10 @wrap never = "test" " " var " " body;
impl syntax (test var body) = `(
    let $var = 0 :: Int32;
    $body
);

dbg.print(include_ast (test x `(x)));
