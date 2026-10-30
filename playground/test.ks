const foo = () => (
    panic("hi");
);

for i in 0..std.sys.argc() do (
    dbg.print(std.sys.argv_at(i));
);

for (i :: Int32) in 0..10 do (
    foo();
);
