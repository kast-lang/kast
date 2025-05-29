use std.prelude.*;

let f = forall[T] { fn(x :: T) -> _ { x, x } };
dbg <| f[int32] (123 :: int32)
