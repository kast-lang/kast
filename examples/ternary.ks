use std.*;

syntax ternary -> 13.1 = condition "?" then_case ":" else_case;
impl syntax ternary = macro (.condition, .then_case, .else_case) => `(
    if $condition then $then_case else $else_case
);

let x :: int32 = true ? 1 : 0;
dbg x;
