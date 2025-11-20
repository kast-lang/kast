use std.prelude.*;
# id
const id = [T] x :: T => x;
print (id[_] "hello, world!");
print (id[_] 1 |> int32_to_string);
# const 
const @"const" = [A, B] a :: A => (_ :: B => a);
let const2 = @"const"[_, _] 2;
print (const2 "hello" |> int32_to_string);
const @"const_v2" = [A] a :: A => ([B] _ :: B => a);
let const3 = @"const_v2"[_] 3;
print (const3[_] "hello");
# mutual recursion
(
    module:
    let f = [F] x :: F => g[F] x;
    f[int32];
    let g = [G] x :: G => f[G] x;
    g[int32];
);
