use std.prelude.*;
# id
const id = [T] x :: T => x;
print (id "hello, world!");
print (id 1 |> to_string);
# const 
const @"const" = [A, B] a :: A => (_ :: B => a);
let const2 = @"const"[_, _] 2;
print (const2 "hello" |> parse);
const @"const_v2" = [A] a :: A => ([B] _ :: B => a);
let const3 = @"const_v2" 3;
# print (const3 "hello"); # type error
# mutual recursion
(
    module:
    let f = [F] x :: F => g[F] x;
    let g = [G] x :: G => f[G] x;
);
