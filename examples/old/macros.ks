let x = 123 :: Int32;

const ast = if typeof x == Int32 then (
    `(print("Int32"))
) else (
    `(panic("AAAAA"))
);
include_ast ast;

const dbg_print = (ast) => `(
    dbg.print(`($ast));
    dbg.print($ast);
);

dbg_print!(2 + 2)
