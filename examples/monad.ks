use std.prelude.*;

const Functor = [F] newtype (
    .fmap :: [A, B] (A -> B, F[A]) -> [F[B]],
);
