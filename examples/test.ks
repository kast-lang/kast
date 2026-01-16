const f = [T] (x :: T) => (
    (() => dbg.print[_](x))();
);

for _ in 0..100 do (
    f(1 :: Int32);
);
